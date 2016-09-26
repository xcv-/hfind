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
  , parseCmdLine
  ) where

import Control.Monad              (forM, (<=<))
import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.Morph        (hoist, generalize)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import qualified System.Posix.Process.ByteString as Posix
import System.IO (hFlush, stdout)

import Text.Parsec (SourceName)

import System.HFind.Path (Dir)

import System.HFind.Types (FSNodeType(Resolved), FSNode(..),
                           FSAnyNode(..),
                           ListEntry(..),
                           NodeListEntry, NodeListEntryR)

import System.HFind.Expr.Types.Value (coerceToString)

import System.HFind.Expr.Baker     (Baker, BakerT)
import System.HFind.Expr.Eval      (Eval)
import System.HFind.Expr.Types.AST (Name)

import qualified System.HFind.Expr.Parser as Parser
import qualified System.HFind.Expr.Baker  as Baker
import qualified System.HFind.Expr.Eval   as Eval

import System.HFind.Expr.Bakers.Fused (ExprBaker, runExprBaker,
                                       runPredBaker)

type NodeAction a   = FSAnyNode     'Resolved -> Eval a
type DirAction a    = FSNode Dir    'Resolved -> Eval a
type EntryAction a  = NodeListEntry 'Resolved -> Eval a

type NodePredicate  = NodeAction  Bool
type DirPredicate   = DirAction   Bool
type EntryPredicate = EntryAction Bool
type EntryCommand   = EntryAction (IO ())


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

parseLetBinding :: SourceName -> String -> M (EntryAction ())
parseLetBinding name s = do
    p <- parseWith runLetBinding Parser.parseLetBinding name s

    return (p . entryToNode)
  where
    runLetBinding :: (Name, ExprBaker) -> Baker (NodeAction ())
    runLetBinding (ident, exprBaker) = do
        varId <- Baker.newVar ident
        expr  <- runExprBaker exprBaker
        return (Eval.setVarValue varId <=< expr)

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

