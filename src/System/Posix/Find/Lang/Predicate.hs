{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Posix.Find.Lang.Predicate
  ( DirPredicate
  , EntryPredicate
  , parsePrunePredicate
  , parseFilterPredicate
  ) where

import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.Morph        (hoist, generalize)

import qualified Data.Text as T

import System.Posix.Text.Path (Dir)

import System.Posix.Find.Lang.Context (BakerT, Eval)
import System.Posix.Find.Lang.Eval    (runPredBaker)
import System.Posix.Find.Lang.Parser  (parsePred)
import System.Posix.Find.Types        (FSNodeType(Resolved), FSNode(..),
                                       FSAnyNode(..),
                                       ListEntry(..), NodeListEntry)


type NodePredicate  = FSAnyNode     'Resolved -> Eval Bool
type DirPredicate   = FSNode Dir    'Resolved -> Eval Bool
type EntryPredicate = NodeListEntry 'Resolved -> Eval Bool


parseNodePredicate :: String -> BakerT (Except String) NodePredicate
parseNodePredicate s =
    case parsePred s (T.pack s) of
        Right mp -> hoist generalize (runPredBaker mp)
        Left err -> lift (throwE (show err))


parsePrunePredicate :: String -> BakerT (Except String) DirPredicate
parsePrunePredicate s = do
    p <- parseNodePredicate s

    return (p . AnyNode)



parseFilterPredicate :: String -> BakerT (Except String) EntryPredicate
parseFilterPredicate s = do
    p <- parseNodePredicate s

    return $ \case
        DirEntry n  -> p (AnyNode n)
        FileEntry n -> p (AnyNode n)
