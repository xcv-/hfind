{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Posix.Find.Lang.BakeParse
  ( DirPredicate
  , EntryPredicate
  , parsePrunePredicate
  , parseFilterPredicate
  , parseStringInterp
  , parseCmdLine
  ) where

import Control.Monad              (forM)
import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.Morph        (hoist, generalize)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import qualified System.Posix.Process.ByteString as Posix
import System.IO (hFlush, stdout)

import Text.Parsec (SourceName)

import System.Posix.Text.Path (Dir)

import System.Posix.Find.Types (FSNodeType(Resolved), FSNode(..),
                                FSAnyNode(..),
                                ListEntry(..),
                                NodeListEntry, NodeListEntryR)

import System.Posix.Find.Lang.Types.Value (coerceToString)

import System.Posix.Find.Lang.Baker (Baker, BakerT)
import System.Posix.Find.Lang.Eval  (Eval)

import qualified System.Posix.Find.Lang.Parser as Parser
import qualified System.Posix.Find.Lang.Baker  as Baker

import System.Posix.Find.Lang.Interp (runPredBaker, runExprBaker)

type NodePredicate  = FSAnyNode     'Resolved -> Eval Bool
type DirPredicate   = FSNode Dir    'Resolved -> Eval Bool
type EntryPredicate = NodeListEntry 'Resolved -> Eval Bool
type EntryCommand   = NodeListEntry 'Resolved -> Eval (IO ())

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
parseNodePredicate = parseWith runPredBaker Parser.parsePred

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
        fmap (\f -> fmap coerceToString . f . entryToNode)
             (parseWith runExprBaker Parser.parseStringInterp name s)

parseCmdLine :: [(SourceName, String)] -> M EntryCommand
parseCmdLine inputs = do
    args <- forM inputs $ \(name, s) -> do
                arg <- parseWith runExprBaker Parser.parseCmdLineArg name s

                return $ \entry ->
                    fmap (T.encodeUtf8 . coerceToString) (arg entry)

    return $ \entry -> do
        let node = entryToNode entry

        (cmd:argv) <- mapM ($ node) args

        return $ do
            let searchInPath = True
                environment  = Nothing

            hFlush stdout

            pid <- Posix.forkProcess $
                       Posix.executeFile cmd searchInPath argv environment
            wait pid

            hFlush stdout
  where
    wait pid = do
        let block    = True
            waitStop = False

        -- TODO: do something if the process crashes
        _ <- Posix.getProcessStatus block waitStop pid

        return ()

