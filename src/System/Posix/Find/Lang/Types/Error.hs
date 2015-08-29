module System.Posix.Find.Lang.Types.Error where

import Data.Text (Text)


import System.Posix.Text.Path (RawPath)

import System.Posix.Find.Lang.Types.AST (Var)
import System.Posix.Find.Lang.Types.Value


-- TODO: error location info

data VarNotFoundError = VarNotFound       !Var
                      | CaptureOutOfRange !Int
    deriving Show

data RuntimeError = !Text `ExpectedButFound` !Text
                  | NotFound      !RawPath
                  | InvalidPathOp !RawPath !Text
    deriving Show

expectedButFound :: ValueType -> ValueType -> RuntimeError
t `expectedButFound` t' = typeName t `ExpectedButFound` typeName t'
