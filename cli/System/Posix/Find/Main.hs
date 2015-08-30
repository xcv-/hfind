{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module System.Posix.Find.Main (main) where

import Control.Monad              (when, forM_)
import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Except (ExceptT, Except, runExceptT, throwE)
import Control.Monad.Morph        (hoist, generalize)
import Control.Monad.IO.Class     (liftIO)

import qualified Control.Exception as Ex

import Data.Monoid ((<>))

import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import qualified Data.Text.ICU as ICU

import System.Environment (getArgs)
import System.Exit        (die, exitFailure)
import System.IO          (stderr)

import Pipes ((>->))
import qualified Pipes         as Pipes
import qualified Pipes.Prelude as Pipes

import System.Posix.Text.Path (Path, Abs, Dir,
                               asDirPath, canonicalizeFromHere)

import qualified System.Posix.Find.Walk        as Walk
import qualified System.Posix.Find.Combinators as C

import System.Posix.Find.Lang.Types.AST   (Var(..))
import System.Posix.Find.Lang.Types.Value (coerceToString)

import qualified System.Posix.Find.Lang.Error as Err

import System.Posix.Find.Lang.Context (BakerT, runBakerT, Eval, runEvalT,
                                       Evaluator)

import System.Posix.Find.Lang.Predicate (parseFilterPredicate,
                                         parsePrunePredicate)

import Text.RawString.QQ (r)


tshow :: Show a => a -> T.Text
tshow = T.pack . show


data Transforms = Transforms
    { treeTransform     :: !(C.TransformR      Eval)
    , listTransform     :: !(C.EntryTransformR Eval)
    , hasTreeTransforms :: !Bool
    }


checkHelpArg :: [String] -> IO ()
checkHelpArg args =
    when ("-h" `elem` args || "--help" `elem` args) $
        die usage


parseLinkStratArg :: Int -> [String] -> (Bool, Int, [String])
parseLinkStratArg i args =
    case args of
        "-L":args' -> (True,  i+1, args')
        args'      -> (False, i+0, args')


parsePathArg :: Int
             -> [String]
             -> ExceptT String IO (Path Abs Dir, Int, [String])
parsePathArg _ [] = throwE "Expected search path"
parsePathArg i (path:args') = do
    mcanonicalized <- liftIO $ canonicalizeFromHere (T.pack path)

    case mcanonicalized of
        Just p  -> return (asDirPath p, i+1, args')
        Nothing -> throwE $ "Invalid path: " ++ path


parseActions :: Int -> [String] -> BakerT (Except String) Transforms
parseActions i ("-if":expr:opts) = do
    predicate  <- parseFilterPredicate ("argument #" ++ show (i+1)) expr
    transforms <- parseActions (i+2) opts

    when (hasTreeTransforms transforms) $
        lift.throwE $ "cannot perform tree actions (like -prune) after a \
                      \list action (like -if)"

    return transforms {
        listTransform = Pipes.filterM predicate >-> listTransform transforms
    }

parseActions i ("-prune":expr:opts) = do
    predicate  <- parsePrunePredicate ("argument #" ++ show (i+1)) expr
    transforms <- parseActions (i+2) opts

    return transforms {
        hasTreeTransforms = True,
        treeTransform = C.pruneDirsM predicate >-> treeTransform transforms
    }

parseActions _ (opt:_) = lift $ throwE ("Invalid action: " ++ opt)
parseActions _ []      = return (Transforms Pipes.cat Pipes.cat False)


parseArgs :: ExceptT String IO (Bool, Path Abs Dir, Transforms, Evaluator)
parseArgs = do
    args <- liftIO getArgs

    liftIO $ checkHelpArg args

    let (links, i', args') = parseLinkStratArg 1 args

    (path, i'', args'') <- parsePathArg i' args'

    let bakerT = hoist (hoist generalize) (parseActions i'' args'')
    (res, vars, activeRx) <- runBakerT path bakerT

    case res of
        Right (trns, evaluator) ->
            return (links, path, trns, evaluator)

        Left (Err.VarNotFound notFoundVar, bt) -> do
            let varStr = case notFoundVar of
                             RxCapVar i -> tshow i
                             NamedVar n -> n

            let msg = "Variable $" <> varStr <> " not found"

            let putErr = liftIO . TIO.hPutStrLn stderr

            putErr (Err.renderError (Err.errorWithBacktrace msg bt))

            putErr ""
            putErr "Defined variables:"
            forM_ vars $ \var ->
                putErr ("    $" <> var)

            case activeRx of
                Nothing -> return ()
                Just rx -> do
                    putErr ""
                    putErr ("Active regex: " <> tshow (ICU.pattern rx))

            throwE ""


main :: IO ()
main = do
    (links, root, trns, evaluator) <- runExceptT parseArgs >>= \case
        Right res -> return res
        Left err | null err  -> exitFailure
                 | otherwise -> die $ "error parsing arguments: " ++ err

    let tree
         | links     = Walk.walk Walk.followSymlinks   root >-> C.followLinks
         | otherwise = Walk.walk Walk.symlinksAreFiles root

    (res, varDump, activeMatch) <-
        runEvalT evaluator $ Pipes.runEffect $ tree
            >-> C.onError C.report
            >-> treeTransform trns
            >-> C.flatten
            >-> listTransform trns
            >-> C.asPaths
            >-> C.asFiles
            >-> C.stringPaths
            >-> Pipes.stdoutLn

    case res of
        Right ()       -> return ()
        Left (err, bt) -> do
            let msg = case err of
                  Err.ExpectedButFound t1 t2 ->
                      "Type error: '" <> t1 <> "' expected, but found "
                               <> "'" <> t2 <> "'"
                  Err.NotFound path ->
                      "File/Directory not found: '" <> path <> "'"
                  Err.InvalidPathOp path op ->
                      "Invalid path operation: trying to perform '"
                          <> op <> "' on '" <> path <> "'"
                  Err.NativeError e ->
                      "Native exception raised during evaluation: " <> tshow e

            let putErr = TIO.hPutStrLn stderr

            putErr (Err.renderError (Err.errorWithBacktrace msg bt))

            putErr ""
            putErr "Variable dump:"

            forM_ varDump $ \(var, value) ->
                putErr ("    $" <> var <> " = " <> tshow (coerceToString value))
                `Ex.catch` \(e :: Ex.SomeException) ->
                    putErr ("    $" <> var <> " = <error> " <> tshow e)

            case activeMatch of
                Nothing    -> return ()
                Just match -> do
                    putErr ""
                    putErr ("Active match: " <> tshow (ICU.pattern match))

                    forM_ [0 .. ICU.groupCount match] $ \i ->
                        putErr ("    $" <> tshow i <> " = "
                                        <> maybe "<error>" tshow
                                             (ICU.group i match))




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

