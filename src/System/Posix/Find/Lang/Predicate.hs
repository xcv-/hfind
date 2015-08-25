{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Posix.Find.Lang.Predicate
  ( DirPredicate
  , EntryPredicate
  , parsePrunePredicate
  , parseFilterPredicate
  ) where

import Control.Monad.Except
import Control.Monad.Morph

import qualified Data.Text as T

import System.Posix.Text.Path (Dir)

import System.Posix.Find.Lang.Context (ScanT, EvalT)
import System.Posix.Find.Lang.Eval    (runPredScan)
import System.Posix.Find.Lang.Parser  (parsePred)
import System.Posix.Find.Types        (FSNodeType(Resolved), FSNode(..),
                                       FSAnyNode(..),
                                       ListEntry(..), NodeListEntry)


type NodePredicate  = FSAnyNode     'Resolved -> EvalT IO Bool
type DirPredicate   = FSNode Dir    'Resolved -> EvalT IO Bool
type EntryPredicate = NodeListEntry 'Resolved -> EvalT IO Bool


parseNodePredicate :: String -> ScanT (ExceptT String IO) NodePredicate
parseNodePredicate s =
    case parsePred s (T.pack s) of
        Right mp -> hoist lift (runPredScan mp)
        Left err -> lift $ throwError (show err)


parsePrunePredicate :: String -> ScanT (ExceptT String IO) DirPredicate
parsePrunePredicate s = do
    p <- parseNodePredicate s

    return (p . AnyNode)


parseFilterPredicate :: String -> ScanT (ExceptT String IO) EntryPredicate
parseFilterPredicate s = do
    p <- parseNodePredicate s

    return $ \case
        DirEntry n  -> p (AnyNode n)
        FileEntry n -> p (AnyNode n)
