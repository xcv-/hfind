{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module System.HFind.Expr.Bakers
  ( DirPredicate
  , EntryPredicate
  , parsePrunePredicate
  , parseFilterPredicate
  , parseLetBinding
  , parseStringInterp
  , parseExecCmdLine
  ) where

import Control.Monad              (forM)
import Control.Monad.Except       (throwError)
import Control.Monad.Morph        (hoist, generalize)
import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Except (Except, throwE)

import qualified Data.Text as T

import Text.Parsec (SourceName)

import System.HFind.Path (Dir)
import qualified System.HFind.Path as Path

import System.HFind.Types (FSNodeType(Resolved), FSNode(..), FSAnyNode(..),
                           ListEntry(..), NodeListEntry, NodeListEntryR,
                           nodePath)

import System.HFind.Expr.Baker (Baker, BakerT)
import System.HFind.Expr.Error (RuntimeError(InvalidPathOp))
import System.HFind.Expr.Eval  (Eval)

import qualified System.HFind.Expr.Baker  as Baker
import qualified System.HFind.Expr.Parser as Parser

import System.HFind.Expr.Bakers.Fused (runFusedBaker, runFusedBakerToString,
                                       bakeLetBinding)

import System.HFind.Exec (forkExecOrThrow, forkExecDirOrThrow)

type NodeAction a   = FSAnyNode     'Resolved -> Eval a
type DirAction a    = FSNode Dir    'Resolved -> Eval a
type EntryAction a  = NodeListEntry 'Resolved -> Eval a

type NodePredicate  = NodeAction  Bool
type DirPredicate   = DirAction   Bool
type EntryPredicate = EntryAction Bool


type M = BakerT (Except String)


entryToNode :: NodeListEntry s -> FSAnyNode s
entryToNode (DirEntry n)  = AnyNode n
entryToNode (FileEntry n) = AnyNode n


parseWith :: (a -> Baker b)
          -> (SourceName -> T.Text -> Either Parser.ParseError a)
          -> SourceName
          -> String
          -> M b
parseWith run parse name s =
    case parse name (T.pack s) of
        Right ma -> hoist generalize (run ma)
        Left err -> lift (throwE (show err))

parseNodePredicate :: SourceName -> String -> M NodePredicate
parseNodePredicate = parseWith runFusedBaker Parser.parsePred

parsePrunePredicate :: SourceName -> String -> M DirPredicate
parsePrunePredicate name s = do
    p <- Baker.readonly $ parseNodePredicate name s

    return (p . AnyNode)

parseFilterPredicate :: SourceName -> String -> M EntryPredicate
parseFilterPredicate name s = do
    p <- parseNodePredicate name s

    return (p . entryToNode)

parseStringInterp :: SourceName -> String -> M (NodeListEntryR -> Eval T.Text)
parseStringInterp name s =
    Baker.readonly $
        fmap (. entryToNode)
             (parseWith runFusedBakerToString Parser.parseStringInterp name s)


parseLetBinding :: SourceName -> String -> M (EntryAction ())
parseLetBinding name s = do
    p <- parseWith (uncurry bakeLetBinding) Parser.parseLetBinding name s

    return (p . entryToNode)

parseExecCmdLine :: SourceName -> Bool -> [(SourceName, String)] -> M (EntryAction ())
parseExecCmdLine name isExecDir inputs = do
    argv <- forM inputs $ \(s_name, s) ->
                parseWith runFusedBakerToString Parser.parseCmdLineArg s_name s

    case argv of
        cmd:args ->
            return $ \entry -> do
                let node = entryToNode entry
                tcmd  <- cmd node
                targs <- mapM ($ node) args
                forkExec tcmd targs node
        [] ->
            lift $ throwE (name ++ ": Program name was not provided")
  where
    forkExec :: T.Text -> [T.Text] -> NodeAction ()
    forkExec tcmd targs (AnyNode node)
      | isExecDir = do
          let path = nodePath node
          case Path.parent path of
              Just parentPath -> forkExecDirOrThrow parentPath tcmd targs
              Nothing         -> throwError (InvalidPathOp (Path.toText path) "parent")
      | otherwise =
          forkExecOrThrow tcmd targs
