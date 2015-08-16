{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module System.Posix.Find.Main (main) where

import Control.Monad (when)
import Control.Monad.Trans.Except

import qualified Data.Text as T

import System.Environment
import System.Exit

import qualified Pipes.Prelude as P

import System.Posix.Find
import System.Posix.Find.Lang.Context (ScanT, runScanT, EvalT, runEvalT,
                                       Evaluator)

import Text.RawString.QQ (r)


type EntryTransform m = Pipe (NodeListEntry 'Resolved) (NodeListEntry 'Resolved) m ()

data Transforms = Transforms
    { treeTransform     :: !(TransformR     (EvalT IO))
    , listTransform     :: !(EntryTransform (EvalT IO))
    , hasTreeTransforms :: !Bool
    }

parseLinkStratArg :: [String] -> (Bool, [String])
parseLinkStratArg args =
    case args of
        "-L":args' -> (True,  args')
        args'      -> (False, args')

parsePathArg :: [String] -> ExceptT String IO (Path Abs Dir, [String])
parsePathArg [] = throwE "Expected search path"
parsePathArg (path:args') = do
    ecanonicalized <- liftIO $ canonicalizeFromHere (T.pack path)

    case ecanonicalized of
        Right p -> return (asDirPath p, args')
        Left p  -> throwE $ "Invalid path: " ++ T.unpack p


parseActions :: [String] -> ScanT (ExceptT String IO) Transforms
parseActions ("-if":expr:opts) = do
    predicate  <- parseFilterPredicate expr
    transforms <- parseActions opts

    when (hasTreeTransforms transforms) $
        lift.throwE $ "cannot perform tree actions (like -prune) after a \
                      \list action (like -if)"

    return transforms {
        listTransform =
            P.filterM (evalEntryPredicate predicate)
                >-> listTransform transforms
    }

parseActions ("-prune":expr:opts) = do
    predicate  <- parsePrunePredicate expr
    transforms <- parseActions opts

    return transforms {
        hasTreeTransforms = True,
        treeTransform =
            pruneDirsM (evalDirPredicate predicate)
                >-> treeTransform transforms
    }

parseActions (opt:_) = lift $ throwE ("Invalid action: " ++ opt)
parseActions []      = return (Transforms cat cat False)


parseArgs :: ExceptT String IO (Bool, Path Abs Dir, Transforms, Evaluator)
parseArgs = do
    args <- liftIO getArgs

    let (links, args') = parseLinkStratArg args

    (path, args'') <- parsePathArg args'

    res <- runScanT path (parseActions args'')

    case res of
        Right (trns, evaluator) ->
            return (links, path, trns, evaluator)

        Left err -> liftIO $ do
            let errmsg = show err

            putStrLn $ "error parsing arguments: " ++ show errmsg
            putStrLn ""
            die usage


main :: IO ()
main = do
    (links, path, trns, evaluator) <- runExceptT parseArgs >>= \case
        Right res -> return res
        Left  err -> do
            putStrLn err
            putStrLn ""
            die usage

    let tree
         | links     = (ls followSymlinks   path >>= yield) >-> followLinks
         | otherwise = (ls symlinksAreFiles path >>= yield)

    res <- runEvalT evaluator $ runEffect $ tree
               >-> onError report
               >-> treeTransform trns
               >-> flatten
               >-> listTransform trns
               >-> asPaths
               >-> asFiles
               >-> stringPaths
               >-> P.stdoutLn

    case res of
        Right () -> return ()
        Left e   -> die (show e)



usage :: String
usage = [r|usage: hfind [-L] <path> [actions...]

flags:
  -L           follow symlinks

supported actions:
  -if pred:    allow only entries satisfying pred
  -prune pred: filter subtrees with root matching pred

predicate syntax:
  expr

  not (pred)

  pred1 && pred2        pred1 || pred2

  expr1 == expr2        expr1 != expr2
  expr1 >= expr2        expr1 <= expr2
  expr1 >  expr2        expr1 <  expr2

  expr =~ m/pcre regex/mxsin

  valid regex delimiters:
    / _ @ % # ! $ €

  regex options:
    m: multiline               s: dot matches newlines
    x: allow spaces/comments   i: case insensitive
    n: do not capture

expression syntax:
  $var
  lit
  "interpolated string with $variables, escape dollars with $$"

variable syntax:
  $identifier
  $previous_regex_capture_index

builtin (magic) variables:
  $name:   file name
  $path:   absolute path
  $hidden: whether $name starts with '.'

literal syntax:
  "string"
  12312319283791283 (64-bit integer)
  true
  false |]

