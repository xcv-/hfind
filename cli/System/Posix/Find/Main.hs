{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module System.Posix.Find.Main (main) where

import Control.Monad.Trans.Except
import Control.Monad.Except

import qualified Data.Text as T

import System.Environment
import System.Exit

import qualified Pipes.Prelude as P

import System.Posix.Find


type EntryTransform m = Pipe (NodeListEntry 'Resolved) (NodeListEntry 'Resolved) m ()


parsePathArg :: String -> ExceptT String IO (Path Abs Dir)
parsePathArg path = do
    ecanonicalized <- liftIO $ canonicalizeFromHere (T.pack path)

    case ecanonicalized of
        Right p -> return (asDirPath p)
        Left p  -> throwError ("Invalid path: " ++ T.unpack p)


parseActions :: Monad m
             => [String]
             -> ExceptT String IO (TransformR m, EntryTransform m)
parseActions ("-if":expr:opts) = do
    predicate <- parseFilterPredicate expr
    (lsP, eP) <- parseActions opts

    return ( lsP
           , P.filterM (evalEntryPredicate predicate) >-> eP )

parseActions ("-prune":expr:opts) = do
    predicate <- parsePrunePredicate expr
    (lsP, eP) <- parseActions opts

    return ( pruneDirsM (evalDirPredicate predicate) >-> lsP
           , eP )

parseActions (opt:_) = throwError ("Invalid action: " ++ opt)
parseActions []      = return (cat, cat)


parseArgs :: Monad m
          => [String]
          -> ExceptT String IO (Path Abs Dir, TransformR m, EntryTransform m)
parseArgs [] = throwError "Expected starting path"
parseArgs (pathArg:opts) = do
    path      <- parsePathArg pathArg
    (lsP, eP) <- parseActions opts

    return (path, lsP, eP)



main :: IO ()
main = do
    prog <- getProgName

    (links, args) <- getArgs <&> \case
                         "-L":args' -> (True,  args')
                         args'      -> (False, args')

    (path, lsP, eP) <- runExceptT (parseArgs args) >>= \case
                           Right r  -> return r
                           Left err -> die $ unlines
                              [ "Error parsing arguments: " ++ err
                              , ""
                              , "usage: " ++ prog ++ " <path> [actions...]"
                              , "  actions:"
                              , "    -if expr:    allow only entries matching expr"
                              , "    -prune expr: filter subtrees matching expr"
                              ]

    let listing
         | links     = (ls FollowSymLinks   path >>= yield) >-> followLinks
         | otherwise = (ls SymLinksAreFiles path >>= yield)

    runEffect $
        listing
            >-> onError report
            >-> lsP
            >-> flatten
            >-> eP
            >-> asPaths
            >-> asFiles
            >-> stringPaths
            >-> P.stdoutLn
