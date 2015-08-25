{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Posix.Find.Lang.Parser
    ( parsePred
    , parseExpr
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
import Text.Parsec.Expr
import Text.Parsec.Pos
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Error as PE
import qualified Text.Parsec.Prim  as Prim
import qualified Text.Parsec.Token as Tok

import System.Posix.Find.Lang.Types


parsePred :: IsPred pre => SourceName -> Text -> Either ParseError pre
parsePred = runParser (whitespace *> predicate <* eof) ()

parseExpr :: IsExpr expr => SourceName -> Text -> Either ParseError expr
parseExpr = runParser (whitespace *> expr <* eof) ()


langDef :: Monad m => Tok.GenLanguageDef Text () m
langDef = Tok.LanguageDef
  { Tok.commentStart = ""
  , Tok.commentEnd = ""
  , Tok.commentLine = ""
  , Tok.nestedComments = False
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum <|> char '_'
  , Tok.opStart = Tok.opStart langDef
  , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedOpNames = ["==", "=~", "<=", ">=", "<", ">", "&&", "||"]
  , Tok.reservedNames = ["true", "false", "not"]
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

symbol :: String -> Parser ()
symbol s = Tok.symbol lang s $> ()

reserved :: String -> Parser ()
reserved s = Tok.symbol lang s $> ()

reservedOp :: String -> Parser ()
reservedOp s = Tok.reservedOp lang s $> ()

integerLiteral :: Num a => Parser a
integerLiteral = fromInteger <$> Tok.integer lang

stringLiteral :: Parser Text
stringLiteral = T.pack <$> Tok.stringLiteral lang


predicateValue :: forall pre. IsPred pre => Parser pre
predicateValue = parens predicateValue
             <|> try (negation   <?> "negation")
             <|> try (binaryPred <?> "binary predicate")
             <|> try (freeExpr   <?> "expression")
  where
    negation :: Parser pre
    negation = notP <$> (reserved "not" *> parens predicate)

    binaryPred :: Parser pre
    binaryPred = do
        e1 <- expr

        optionMaybe comparator >>= \case
            Just op -> do
                e2 <- expr
                return (opP op e1 e2)
            Nothing -> do
                symbol "=~"
                (rx, capMode) <- regex
                return (matchP e1 rx capMode)

    freeExpr :: Parser pre
    freeExpr = exprP <$> expr


predicate :: IsPred pre => Parser pre
predicate = buildExpressionParser
    [ [ Infix  (reservedOp "&&" $> andP) AssocLeft
      , Infix  (reservedOp "||" $> orP)  AssocLeft
      ]
    ] predicateValue

comparator :: Parser Op
comparator = try (symbol "==" $> OpEQ)
         <|> try (symbol "<=" $> OpLE)
         <|> try (symbol ">=" $> OpGE)
         <|> try (symbol "<"  $> OpLT)
         <|> try (symbol ">"  $> OpGT)
         <?> "comparison operator"

expr :: forall expr. IsExpr expr => Parser expr
expr = parens expr
   <|> varE        <$> var
   <|> litE        <$> try litNoString
   <|> simplInterp <$> interp
   <|> app
   <?> "expression"
  where
    app :: Parser expr
    app = appE <$> identifier <*> expr <?> "function application"

    simplInterp :: [Either Text (ExprVar expr)] -> expr
    simplInterp pieces =
        case contract pieces of
          []       -> litE (stringL "")
          [Left s] -> litE (stringL s)
          pieces'  -> interpE pieces'

    contract :: [Either Text (ExprVar expr)] -> [Either Text (ExprVar expr)]
    contract [] = []
    contract (Left "" : ps) = contract ps
    contract (p:ps) =
        case (p, contract ps) of
          (Left s1, Left s2 : ps') -> Left (s1 <> s2) : ps'
          (_, ps')                 -> p:ps'

    interp :: Parser [Either Text (ExprVar expr)]
    interp = flip label "interpolated string" $
        either throwParseError return . parseInterp =<< stringLiteral

    parseInterp :: Text -> Either (SourcePos -> ParseError)
                                    [Either Text (ExprVar expr)]
    parseInterp s =
        case T.break (=='$') s of
          (prefix, "") ->
                return [Left prefix]

          (prefix, s')
            | "$$" `T.isPrefixOf` s' ->
                fmap (Left (prefix `T.snoc` '$') :)
                     (parseInterp (T.drop 2 s'))
            | otherwise ->
                case parseWithLeftovers varNoWhitespace "string literal" s' of
                  Right (v, s'') ->
                      fmap ([Left prefix, Right v] ++)
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
    delim <- oneOf "/_@%#!€$"

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
litNoString = boolL True     <$  reserved "true"
          <|> boolL False    <$  reserved "false"
          <|> numL           <$> integerLiteral
          <?> "literal"
  where
    numLit = try date <|> try size <?> "numeric literal"

    date = undefined
    size = undefined


var :: IsVar var => Parser var
var = varNoWhitespace <* whitespace
  <?> "variable"

varNoWhitespace :: IsVar var => Parser var
varNoWhitespace = do
    void (char '$')

    let rawVar = rxCapVar <$> rxCapIndex
             <|> namedVar <$> ident

    between (symbol "{") (char '}') (rawVar <* whitespace)
      <|> rawVar
      <?> "variable name or regex capture index"
  where
    rxCapIndex = do
        digits <- many1 (digitToInt <$> digit)

        return $ foldl' (\acc x -> acc*10 + x) 0 digits

    ident = do
        c  <- Tok.identStart langDef
        cs <- many (Tok.identLetter langDef)
        return (T.pack (c:cs))



-- low-level Text.Parsec.Prim-based utilities

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
