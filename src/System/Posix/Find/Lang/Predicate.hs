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
                                FSAnyNode(..), FSAnyNodeR,
                                ListEntry(..), NodeListEntry)

import System.Posix.Find.Lang.Context (Baker, BakerT, Eval)

import qualified System.Posix.Find.Lang.Context as Ctx (bakeReadonly,
                                                        getOrNewVar, setVarValue)

import System.Posix.Find.Lang.Types.Value (Value(NodeV))

import System.Posix.Find.Lang.Eval    (runPredBaker)
import System.Posix.Find.Lang.Parser  (parsePred)

type NodePredicate  = FSAnyNode     'Resolved -> Eval Bool
type DirPredicate   = FSNode Dir    'Resolved -> Eval Bool
type EntryPredicate = NodeListEntry 'Resolved -> Eval Bool

type M = BakerT (Except String)


parseNodePredicate :: SourceName -> String -> M NodePredicate
parseNodePredicate name s =
    case parsePred name (T.pack s) of
        Right mp -> hoist generalize (setCurrent $ runPredBaker mp)
        Left err -> lift (throwE (show err))
  where
    setCurrent :: Baker (FSAnyNodeR -> Eval Bool)
               -> Baker (FSAnyNodeR -> Eval Bool)
    setCurrent bake = do
        currentNodeVar <- Ctx.getOrNewVar "_currentnode"
        predicate <- bake

        return $ \n -> do
            Ctx.setVarValue currentNodeVar (NodeV n)
            predicate n


parsePrunePredicate :: SourceName -> String -> M DirPredicate
parsePrunePredicate name s = do
    p <- Ctx.bakeReadonly $ parseNodePredicate name s

    return (p . AnyNode)



parseFilterPredicate :: SourceName -> String -> M EntryPredicate
parseFilterPredicate name s = do
    p <- parseNodePredicate name s

    return $ \case
        DirEntry n  -> p (AnyNode n)
        FileEntry n -> p (AnyNode n)
