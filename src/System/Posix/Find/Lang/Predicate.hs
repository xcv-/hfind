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

import Text.Parsec (SourceName)

import System.Posix.Text.Path (Dir)

import System.Posix.Find.Types (FSNodeType(Resolved), FSNode(..),
                                FSAnyNode(..),
                                ListEntry(..), NodeListEntry)

import System.Posix.Find.Lang.Baker (BakerT)
import System.Posix.Find.Lang.Eval  (Eval)

import qualified System.Posix.Find.Lang.Baker as Baker

import System.Posix.Find.Lang.Interp  (runPredBaker)
import System.Posix.Find.Lang.Parser  (parsePred)

type NodePredicate  = FSAnyNode     'Resolved -> Eval Bool
type DirPredicate   = FSNode Dir    'Resolved -> Eval Bool
type EntryPredicate = NodeListEntry 'Resolved -> Eval Bool

type M = BakerT (Except String)


parseNodePredicate :: SourceName -> String -> M NodePredicate
parseNodePredicate name s =
    case parsePred name (T.pack s) of
        Right mp -> hoist generalize (runPredBaker mp)
        Left err -> lift (throwE (show err))

parsePrunePredicate :: SourceName -> String -> M DirPredicate
parsePrunePredicate name s = do
    p <- Baker.readonly $ parseNodePredicate name s

    return (p . AnyNode)



parseFilterPredicate :: SourceName -> String -> M EntryPredicate
parseFilterPredicate name s = do
    p <- parseNodePredicate name s

    return $ \case
        DirEntry n  -> p (AnyNode n)
        FileEntry n -> p (AnyNode n)
