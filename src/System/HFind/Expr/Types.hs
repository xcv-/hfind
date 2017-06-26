module System.HFind.Expr.Types
  ( module X
  , TypedName(..)
  ) where

import qualified Data.Text as T

import System.HFind.Expr.Types.AST   as X
import System.HFind.Expr.Types.Value as X

data TypedName = TypedName !X.ValueType !X.Name

instance Show TypedName where
    show (TypedName ty name) = show ty ++ " " ++ T.unpack name
