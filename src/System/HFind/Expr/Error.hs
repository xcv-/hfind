{-# LANGUAGE OverloadedStrings #-}
module System.HFind.Expr.Error where

import Control.Exception (SomeException)

import Data.Monoid ((<>))

import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict, pack)

import Text.Parsec.Pos (SourcePos)

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import System.HFind.Path (RawPath)

import System.HFind.Expr.Types.AST (Src, Var)
import System.HFind.Expr.Types.Value


data VarNotFoundError = VarNotFound !Var
    deriving Show

data RuntimeError = !Text `ExpectedButFound` !Text
                  | NotFound      !RawPath
                  | InvalidPathOp !RawPath !Text
                  | NativeError   !SomeException
    deriving Show


expectedButFound :: ValueType -> ValueType -> RuntimeError
t `expectedButFound` t' = typeName t `ExpectedButFound` typeName t'


data BtFrame = BtFrame { frameDesc :: Text
                       , frameSrc  :: Src
                       , frameLoc  :: SourcePos
                       }

newtype Backtrace = Backtrace { getFrames :: [BtFrame] }

emptyBacktrace :: Backtrace
emptyBacktrace = Backtrace []

pushFrame :: BtFrame -> Backtrace -> Backtrace
pushFrame frame (Backtrace frames) = Backtrace (frame:frames)

popFrame :: Backtrace -> Backtrace
popFrame (Backtrace (_:frames)) = Backtrace frames
popFrame (Backtrace []) =
    error "System.Posix.Find.Lang.Types.Error.popFrame: Empty back trace"


renderError :: Doc -> Text
renderError = toStrict . PP.displayT . PP.renderPretty 0.5 90


errorWithBacktrace :: Text -> Backtrace -> Doc
errorWithBacktrace errmsg bt =
    formatError errmsg bt
       PP.<$> indentBacktrace (formatBacktrace bt)

indentBacktrace :: Doc -> Doc
indentBacktrace = PP.indent 4


formatError :: Text -> Backtrace -> Doc
formatError errmsg (Backtrace frames) =
    let pos = case frames of
                  [] -> mempty
                  _  -> prettyPos (frameLoc (head frames))
    in PP.text "Error" <> pos <> PP.colon
           PP.<+> PP.text (fromStrict errmsg)
  where
    prettyPos :: SourcePos -> Doc
    prettyPos srcPos =
        PP.space <> PP.text "at" PP.<+> PP.text (pack (show srcPos))


formatBacktrace :: Backtrace -> Doc
formatBacktrace (Backtrace frames) = PP.vsep (map pprint frames)
  where
    pprint :: BtFrame -> Doc
    pprint (BtFrame desc src _) =
        PP.text ("In " <> fromStrict desc) <> PP.colon
            PP.<+> PP.text (fromStrict src)
