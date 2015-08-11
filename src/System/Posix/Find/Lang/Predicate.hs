{-# LANGUAGE OverloadedStrings #-}
module System.Posix.Find.Lang.Predicate
  ( DirPredicate(..)
  , EntryPredicate(..)
  , parsePrunePredicate
  , parseFilterPredicate
  ) where

import Control.Monad.Trans.Except

import System.Posix.Find.Lang.Types


parsePrunePredicate :: Monad n => String -> ExceptT String IO (DirPredicate n)
parsePrunePredicate _ = return $ DirPredicate (const (return False))

parseFilterPredicate :: Monad n => String -> ExceptT String IO (EntryPredicate n)
parseFilterPredicate _ = return $ EntryPredicate (const (return True))

{-
parseNodePredicate :: String -> Either String (Maybe PathType, NodePredicate)
parseNodePredicate =
    either show id . parse nodePredicateParser "<argument>"
-}
