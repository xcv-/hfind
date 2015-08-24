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

import Control.Monad.IO.Class

import qualified Data.Text as T

import Data.Text.ICU.Regex (Regex)
import qualified Data.Text.ICU.Error as ICU
import qualified Data.Text.ICU.Regex as Regex

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.Parsec.Error as PE
import qualified Text.Parsec.Prim  as Prim
import qualified Text.Parsec.Token as Tok

import System.Posix.Find.Lang.Types


parsePred :: IsPred pre => SourceName -> T.Text -> IO (Either ParseError pre)
parsePred = runParserT (whitespace *> predicate <* eof) ()

parseExpr :: IsExpr expr => SourceName -> T.Text -> IO (Either ParseError expr)
parseExpr = runParserT (whitespace *> expr <* eof) ()


type ParserT = ParsecT T.Text ()
type Parser  = ParserT IO

langDef :: Monad m => Tok.GenLanguageDef T.Text () m
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

lang :: Monad m => Tok.GenTokenParser T.Text () m
lang = Tok.makeTokenParser langDef

whitespace :: Monad m => ParserT m ()
whitespace = Tok.whiteSpace lang

identifier :: Monad m => ParserT m T.Text
identifier = T.pack <$> Tok.identifier lang

parens :: Monad m => ParserT m a -> ParserT m a
parens = Tok.parens lang

symbol :: Monad m => String -> ParserT m ()
symbol s = Tok.symbol lang s $> ()

reserved :: Monad m => String -> ParserT m ()
reserved s = Tok.symbol lang s $> ()

reservedOp :: Monad m => String -> ParserT m ()
reservedOp s = Tok.reservedOp lang s $> ()

integerLiteral :: (Monad m, Num a) => ParserT m a
integerLiteral = fromInteger <$> Tok.integer lang

stringLiteral :: Monad m => ParserT m T.Text
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

comparator :: Monad m => ParserT m Op
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

    simplInterp :: [Either T.Text (ExprVar expr)] -> expr
    simplInterp pieces =
        case contract pieces of
          []       -> litE (stringL "")
          [Left s] -> litE (stringL s)
          pieces'  -> interpE pieces'

    contract :: [Either T.Text (ExprVar expr)] -> [Either T.Text (ExprVar expr)]
    contract [] = []
    contract (Left "" : ps) = contract ps
    contract (p:ps) =
        case (p, contract ps) of
          (Left s1, Left s2 : ps') -> Left (s1 <> s2) : ps'
          (_, ps')                 -> p:ps'

    interp :: Parser [Either T.Text (ExprVar expr)]
    interp = flip label "interpolated string" $
        either throwParseError return . parseInterp =<< stringLiteral

    parseInterp :: T.Text -> Either (SourcePos -> ParseError)
                                    [Either T.Text (ExprVar expr)]
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
                          err p = foldl' (\e' m -> PE.addErrorMessage m e')
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

    erx <- liftIO $ Regex.regex' opts pattern

    case erx of
      Right rx -> return (rx, cap)
      Left e   -> let err = parseError (ICU.errorName (Regex.errError e))
                  in throwParseError $ \pos ->
                       case Regex.errOffset e of
                         Just off -> err (incSourceColumn pos off)
                         Nothing  -> err pos
  where
    regexOpts :: Parser (RxCaptureMode, [Regex.MatchOption])
    regexOpts = do
        opts <- many letter <* whitespace

        let translate = \case 'm' -> return Regex.Multiline
                              'x' -> return Regex.Comments
                              's' -> return Regex.DotAll
                              'i' -> return Regex.CaseInsensitive
                              c   -> unexpected ("regex option " ++ show c)

        case partition (/='n') opts of
          (opts', []) -> (,) Capture   <$> mapM translate opts'
          (opts', _ ) -> (,) NoCapture <$> mapM translate opts'


litNoString :: IsLit lit => Parser lit
litNoString = boolL True     <$  reserved "true"
          <|> boolL False    <$  reserved "false"
          <|> numL           <$> integerLiteral
          <?> "literal"

var :: (Monad m, IsVar var) => ParserT m var
var = varNoWhitespace <* whitespace
  <?> "variable"

varNoWhitespace :: (Monad m, IsVar var) => ParserT m var
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

parseWithLeftovers :: ParserT Identity a
                   -> SourceName
                   -> T.Text
                   -> Either ParseError (a, T.Text)
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


throwParseError :: Monad m => (SourcePos -> ParseError) -> ParserT m a
throwParseError err =
    Prim.mkPT $ \st ->
      let pos = Prim.statePos st
      in  return (Prim.Empty (return (Prim.Error (err pos))))
