{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module System.Posix.Find.Main (main) where

import Control.Monad              (when)
import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Except (ExceptT, Except, runExceptT, throwE)
import Control.Monad.Morph        (hoist, generalize)
import Control.Monad.IO.Class     (liftIO)

import qualified Data.Text as T

import System.Environment (getArgs)
import System.Exit        (die)

import Pipes ((>->))
import qualified Pipes         as Pipes
import qualified Pipes.Prelude as Pipes

import System.Posix.Text.Path (Path, Abs, Dir,
                               asDirPath, canonicalizeFromHere)

import qualified System.Posix.Find.Walk        as Walk
import qualified System.Posix.Find.Combinators as C

import System.Posix.Find.Lang.Context (BakerT, runBakerT, Eval, runEvalT,
                                       Evaluator)

import System.Posix.Find.Lang.Predicate (parseFilterPredicate,
                                         parsePrunePredicate)

import Text.RawString.QQ (r)


data Transforms = Transforms
    { treeTransform     :: !(C.TransformR      Eval)
    , listTransform     :: !(C.EntryTransformR Eval)
    , hasTreeTransforms :: !Bool
    }


checkHelpArg :: [String] -> IO ()
checkHelpArg args = do
    when ("-h" `elem` args || "--help" `elem` args) $
        die usage


parseLinkStratArg :: [String] -> (Bool, [String])
parseLinkStratArg args =
    case args of
        "-L":args' -> (True,  args')
        args'      -> (False, args')


parsePathArg :: [String] -> ExceptT String IO (Path Abs Dir, [String])
parsePathArg [] = throwE "Expected search path"
parsePathArg (path:args') = do
    mcanonicalized <- liftIO $ canonicalizeFromHere (T.pack path)

    case mcanonicalized of
        Just p  -> return (asDirPath p, args')
        Nothing -> throwE $ "Invalid path: " ++ path


parseActions :: [String] -> BakerT (Except String) Transforms
parseActions ("-if":expr:opts) = do
    predicate  <- parseFilterPredicate expr
    transforms <- parseActions opts

    when (hasTreeTransforms transforms) $
        lift.throwE $ "cannot perform tree actions (like -prune) after a \
                      \list action (like -if)"

    return transforms {
        listTransform = Pipes.filterM predicate >-> listTransform transforms
    }

parseActions ("-prune":expr:opts) = do
    predicate  <- parsePrunePredicate expr
    transforms <- parseActions opts

    return transforms {
        hasTreeTransforms = True,
        treeTransform = C.pruneDirsM predicate >-> treeTransform transforms
    }

parseActions (opt:_) = lift $ throwE ("Invalid action: " ++ opt)
parseActions []      = return (Transforms Pipes.cat Pipes.cat False)


parseArgs :: ExceptT String IO (Bool, Path Abs Dir, Transforms, Evaluator)
parseArgs = do
    args <- liftIO getArgs

    liftIO $ checkHelpArg args

    let (links, args') = parseLinkStratArg args

    (path, args'') <- parsePathArg args'

    res <- runBakerT path $ hoist (hoist generalize) (parseActions args'')

    case res of
        Right (trns, evaluator) ->
            return (links, path, trns, evaluator)

        Left err -> throwE (show err)


main :: IO ()
main = do
    (links, path, trns, evaluator) <- runExceptT parseArgs >>= \case
        Right res -> return res
        Left  err -> die $ "error parsing arguments: " ++ err

    let tree
         | links     = Walk.walk Walk.followSymlinks   path >-> C.followLinks
         | otherwise = Walk.walk Walk.symlinksAreFiles path

    res <- runEvalT evaluator $ Pipes.runEffect $ tree
               >-> C.onError C.report
               >-> treeTransform trns
               >-> C.flatten
               >-> listTransform trns
               >-> C.asPaths
               >-> C.asFiles
               >-> C.stringPaths
               >-> Pipes.stdoutLn

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

