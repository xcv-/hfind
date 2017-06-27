{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module System.HFind.Expr.Parser
    ( parsePred
    , parseExpr
    , parseLetBinding
    , parseStringInterp
    , parseCmdLineArg
    , ParseError
    ) where

import Data.Char
import Data.Functor
import Data.Functor.Identity
import Data.Monoid
import Data.List (foldl', partition)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Text.ICU (Regex)
import qualified Data.Text.ICU       as ICU
import qualified Data.Text.ICU.Error as ICU

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Error as PE
import qualified Text.Parsec.Expr  as PExpr
import qualified Text.Parsec.Prim  as Prim
import qualified Text.Parsec.Token as Tok

import System.HFind.Expr.Types


parsePred :: IsPred pre => SourceName -> Text -> Either ParseError pre
parsePred = runParser (whitespace *> predicate <* eof) ()

parseExpr :: IsExpr expr => SourceName -> Text -> Either ParseError expr
parseExpr = runParser (whitespace *> expr <* eof) ()

parseLetBinding :: IsExpr expr => SourceName -> Text -> Either ParseError (Name, expr)
parseLetBinding = runParser (whitespace *> letBinding <* eof) ()

parseStringInterp :: IsExpr expr => SourceName -> Text -> Either ParseError expr
parseStringInterp = runParser (whitespace *> parser <* eof) ()
  where
    parser = located $ do
        stringInterp =<< consumeEverything

parseCmdLineArg :: IsExpr expr => SourceName -> Text -> Either ParseError expr
parseCmdLineArg = runParser (whitespace *> cmdLineArg <* eof) ()


langDef :: Monad m => Tok.GenLanguageDef Text () m
langDef = Tok.LanguageDef
  { Tok.commentStart = ""
  , Tok.commentEnd = ""
  , Tok.commentLine = ""
  , Tok.nestedComments = False
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum <|> char '_'
  , Tok.opStart = Tok.opStart langDef
  , Tok.opLetter = oneOf ":!#%&*+./<=>?@\\^|-~"
  , Tok.reservedOpNames = ["+", "-", "*", "/",
                           "==", "=~", "<=", ">=", "<", ">"]
  , Tok.reservedNames = ["true", "false", "and", "or", "not", "scope"]
  , Tok.caseSensitive = True
  }

lang :: Monad m => Tok.GenTokenParser Text () m
lang = Tok.makeTokenParser langDef

whitespace :: Parser ()
whitespace = Tok.whiteSpace lang

identifier :: Parser Text
identifier = T.pack <$> Tok.identifier lang

parens :: Parser a -> Parser a
parens = Tok.parens lang

-- braces :: Parser a -> Parser a
-- braces = Tok.braces lang

symbol :: String -> Parser ()
symbol s = Tok.symbol lang s $> ()

reserved :: String -> Parser ()
reserved s = Tok.reserved lang s

reservedOp :: String -> Parser ()
reservedOp s = Tok.reservedOp lang s $> ()

naturalLiteral :: Num a => Parser a
naturalLiteral = fromInteger <$> Tok.natural lang

stringLiteral :: Parser Text
stringLiteral = T.pack <$> Tok.stringLiteral lang


cmdLineArg :: IsExpr expr => Parser expr
cmdLineArg = located $ do
    stringInterp =<< consumeEverything


predicate :: forall pre. IsPred pre => Parser pre
predicate = do
    i1  <- getInput
    loc <- getPosition

    p1  <- predicateValue

    rest <- many (item i1 loc)

    -- [op_rhs2, op_rhs3, ...] -> ... op_rhs3 (op_rhs2 p1)
    return $ foldl' (\lhs op_rhs -> op_rhs lhs) p1 rest
  where
    item :: Src -> SourcePos -> Parser (pre -> pre)
    item i1 loc = try $ do
        op <- reserved "and" $> andP
          <|> reserved "or"  $> orP

        rhs <- predicateValue
        ik  <- getInput

        let src = T.take (T.length i1 - T.length ik) i1

        return (\lhs -> op lhs rhs (SrcLoc src loc))

predicateValue :: forall pre. IsPred pre => Parser pre
predicateValue = parens predicate
             <|> (scope    <?> "explicit scope")
             <|> (negation <?> "negation")
             <|> (exprPred <?> "expression predicate")
             <?> "atomic predicate"
  where
    scope :: Parser pre
    scope = located $
        scopeP <$> (reserved "scope" *> parens predicate)

    negation :: Parser pre
    negation = located $
        notP <$> (reserved "not" *> predicateValue)

    exprPred :: Parser pre
    exprPred = located $ do
        e1 <- expr

        optionMaybe comparator >>= \case
            Just op -> do
                e2 <- expr
                return (opP op e1 e2)

            Nothing -> choice
              [ do reservedOp "=~"
                   (rx, capMode) <- regex
                   return (matchP e1 rx capMode)
              , return (exprP e1)
              ]

comparator :: Parser Op
comparator = (reservedOp "==" $> OpEQ)
         <|> (reservedOp "<=" $> OpLE)
         <|> (reservedOp ">=" $> OpGE)
         <|> (reservedOp "<"  $> OpLT)
         <|> (reservedOp ">"  $> OpGT)
         <?> "comparison operator"

letBinding :: forall expr. IsExpr expr => Parser (Name, expr)
letBinding = do
    ident <- identifier
    whitespace
    symbol "="
    e <- expr
    return (ident, e)


expr :: forall expr. IsExpr expr => Parser expr
expr = located arithExpr

-- HACK: Parsec's expression parser limits operator types to
--   Parser (a -> a -> a),
-- but we need
--   Parser (a -> a -> SrcLoc -> a)
-- or similar, so we use a = (SrcLoc -> expr):
--   Parser ((SrcLoc -> expr) -> (SrcLoc -> expr) -> SrcLoc -> expr)
-- Hopefully most of them will not be used (intermediate values),
-- while the base atoms are actually
--   fmap const (located ...),
-- so they do contain the SrcLoc
arithExpr :: forall expr. IsExpr expr => Parser (SrcLoc -> expr)
arithExpr = PExpr.buildExpressionParser table (fmap const exprAtom)
  where
    table :: PExpr.OperatorTable T.Text () Identity (SrcLoc -> expr)
    table = [ [ prefix pos, prefix neg ]
            , [ binary mult ]
            , [ binary plus, binary minus ] ]

    binary p = PExpr.Infix p PExpr.AssocLeft
    prefix p = PExpr.Prefix p

    plus  = reservedOp "+" $> \x y l -> plusE (x l) (y l) l
    minus = reservedOp "-" $> \x y l -> plusE (x l) (negE (y l) l) l
    mult  = reservedOp "*" $> \x y l -> multE (x l) (y l) l
    pos   = reservedOp "+" $> \x l   -> x l
    neg   = reservedOp "-" $> \x l   -> negE (x l) l

exprAtom :: forall expr. IsExpr expr => Parser expr
exprAtom =
       parens (located arithExpr)
   <|> choice
         [ located $ varE         <$> var
         , located $ stringInterp =<< stringLiteral
         , located $ litE         <$> litNoString
         , located $ app
         ]
   <?> "expression"
  where
    app :: Parser (SrcLoc -> expr)
    app = appE <$> identifier <*> exprAtom <?> "function application"


stringInterp :: forall expr. IsExpr expr => Text -> Parser (SrcLoc -> expr)
stringInterp input = do
    e <- either throwParseError return (parseInterp input)

    return (simplInterp e)
  where
    simplInterp :: [Interp (ExprVar expr)] -> SrcLoc -> expr
    simplInterp pieces =
        case contract pieces of
          []            -> \src -> litE (stringL "" src) src
          [InterpLit s] -> \src -> litE (stringL s src)  src
          pieces'       -> interpE pieces'

    contract :: [Interp (ExprVar expr)] -> [Interp (ExprVar expr)]
    contract [] = []
    contract (InterpLit "" : ps) = contract ps
    contract (p:ps) =
        case (p, contract ps) of
          (InterpLit s1, InterpLit s2 : ps') -> InterpLit (s1 <> s2) : ps'
          (_, ps')                           -> p:ps'

    parseInterp :: Text -> Either (SourcePos -> ParseError) [Interp (ExprVar expr)]
    parseInterp s =
        case T.break (=='$') s of
          (prefix, "") ->
                return [InterpLit prefix]

          (prefix, s')
            | "$$" `T.isPrefixOf` s' ->
                fmap (InterpLit (prefix `T.snoc` '$') :)
                     (parseInterp (T.drop 2 s'))
            | otherwise ->
                case parseWithLeftovers varNoWhitespace "string literal" s' of
                  Right (v, s'') ->
                      fmap ([InterpLit prefix, InterpVar v] ++)
                           (parseInterp s'')
                  Left e ->
                      let msg   = "Could not parse interpolated string"
                          err p = foldr PE.addErrorMessage
                                    (parseError msg p)
                                    (PE.errorMessages e)
                      in Left err


regex :: Parser (Regex, RxCaptureMode)
regex = flip label "regular expression" $ do
    void (char 'm')
    delim <- oneOf "/_@%#!,;|"

    let go acc escaped = do
          c <- anyChar
          case c of
            '\\' | escaped   -> go ('\\':'\\':acc) False
                 | otherwise -> go acc             True

            _ | c == delim -> if escaped then go (c:acc) False
                                         else return acc
              | otherwise  -> if escaped then go (c:'\\':acc) False
                                         else go (c:acc) False

    pattern     <- T.pack . reverse <$> go [] False
    (cap, opts) <- regexOpts

    case ICU.regex' opts pattern of
      Right rx -> return (rx, cap)
      Left e   -> let err = parseError (ICU.errorName (ICU.errError e))
                  in throwParseError $ \pos ->
                       case ICU.errOffset e of
                         Just off -> err (incSourceColumn pos off)
                         Nothing  -> err pos
  where
    regexOpts :: Parser (RxCaptureMode, [ICU.MatchOption])
    regexOpts = do
        opts <- many letter <* whitespace

        let translate = \case 'm' -> return ICU.Multiline
                              'x' -> return ICU.Comments
                              's' -> return ICU.DotAll
                              'i' -> return ICU.CaseInsensitive
                              c   -> unexpected ("regex option " ++ show c)

        case partition (/='n') opts of
          (opts', []) -> (,) Capture   <$> mapM translate opts'
          (opts', _ ) -> (,) NoCapture <$> mapM translate opts'


litNoString :: IsLit lit => Parser lit
litNoString = located (boolL True     <$  reserved "true"
                   <|> boolL False    <$  reserved "false"
                   <|> numL           <$> naturalLiteral
                   <?> "literal")
  where
    numLit = try date <|> try size <?> "numeric literal"

    date = undefined
    size = undefined


var :: IsVar var => Parser var
var = varNoWhitespace <* whitespace
  <?> "variable"

varNoWhitespace :: IsVar var => Parser var
varNoWhitespace = located $ do
    void (char '$')

    let rawVar = rxCapVar <$> rxCapIndex
             <|> namedVar <$> ident
             <?> "variable name or regex capture index"

    between (symbol "{") (char '}') (rawVar <* whitespace)
      <|> rawVar
  where
    rxCapIndex = do
        digits <- many1 (digitToInt <$> digit)
        return $ foldl' (\acc x -> acc*10 + x) 0 digits

    ident = do
        c  <- Tok.identStart langDef
        cs <- many (Tok.identLetter langDef)
        return (T.pack (c:cs))



-- low-level Text.Parsec.Prim-based utilities

located :: Parser (SrcLoc -> a) -> Parser a
located p = do
    loc  <- getPosition
    prev <- getInput
    f    <- p
    cur  <- getInput

    let src = T.take (T.length prev - T.length cur) prev
    return (f (SrcLoc src loc))


consumeEverything :: Parser Text
consumeEverything = do
    input <- getInput
    setInput ""
    return input

parseWithLeftovers :: Parser a
                   -> SourceName
                   -> Text
                   -> Either ParseError (a, Text)
parseWithLeftovers p srcName s = runIdentity $ do
    cons <- Prim.runParsecT p (State s (initialPos srcName) ())

    rep <- case cons of
             Consumed mrep -> mrep
             Empty mrep    -> mrep

    case rep of
      Ok a st _ -> return $ Right (a, Prim.stateInput st)
      Error err -> return $ Left err


parseError :: String -> SourcePos -> ParseError
parseError msg = PE.newErrorMessage (PE.Message msg)


throwParseError :: (SourcePos -> ParseError) -> Parser a
throwParseError err =
    Prim.mkPT $ \st ->
      let pos = Prim.statePos st
      in  return (Prim.Empty (return (Prim.Error (err pos))))
