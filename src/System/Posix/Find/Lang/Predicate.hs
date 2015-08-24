{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Posix.Find.Lang.Predicate
  ( DirPredicate(..)
  , EntryPredicate(..)
  , parsePrunePredicate
  , parseFilterPredicate
  ) where

import Control.Monad.Except
import Control.Monad.Morph

import qualified Data.Text as T

import System.Posix.Find.Lang.Context
import System.Posix.Find.Lang.Eval (runPredScan)
import System.Posix.Find.Lang.Parser
import System.Posix.Find.Types (ListEntry(..), FSAnyNode(..))


parsePrunePredicate :: String -> ScanT (ExceptT String IO) DirPredicate
parsePrunePredicate s = do
    p <- parseFilterPredicate s

    return (DirPredicate (\n -> evalEntryPredicate p (DirEntry n)))


parseFilterPredicate :: String -> ScanT (ExceptT String IO) EntryPredicate
parseFilterPredicate s = do
    r <- liftIO $ parsePred s (T.pack s)

    case r of
        Right mp -> do
            p <- hoist lift (runPredScan mp)
            return $ EntryPredicate $ \case
                DirEntry n  -> p (AnyNode n)
                FileEntry n -> p (AnyNode n)
        Left err -> lift $ throwError (show err)
