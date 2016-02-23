{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module System.HFind.ArgParse where

import Control.Monad              (when, forM, forM_, (<=<))
import Control.Monad.Trans.Except (ExceptT(..), Except, throwE)
import Control.Monad.Morph        (hoist, generalize)
import Control.Monad.IO.Class     (liftIO)

import Data.Monoid ((<>))

import qualified Data.Text     as T
import qualified Data.Text.IO  as TextIO
import qualified Data.Text.ICU as ICU

import System.Exit        (die, exitSuccess)
import System.IO          (stderr)

import Pipes ((>->))
import qualified Pipes         as Pipes
import qualified Pipes.Prelude as Pipes

import Text.RawString.QQ (r)

import System.HFind.Path (Path, Abs, Dir, RawPath)
import qualified System.HFind.Path as Path

import qualified System.HFind.Combinators as C

import System.HFind.Expr.Types.AST (Var(..))

import System.HFind.Expr.Baker (BakerT, runBakerT)
import System.HFind.Expr.Eval  (Eval)

import qualified System.HFind.Expr.Baker as Baker

import System.HFind.Expr.Bakers (parseFilterPredicate,
                                 parsePrunePredicate,
                                 parseStringInterp,
                                 parseCmdLine)

import qualified System.HFind.Expr.Error as Err


data FindFlag = FollowLinks | DryRun
    deriving (Eq, Show)


data Result = Result
    { findFlags     :: [FindFlag]
    , rootPath      :: Path Abs Dir
    , treeContext   :: Baker.BakingContext
    , treeTransform :: C.TransformR Eval
    , listContext   :: Baker.BakingContext
    , listTransform :: C.EntryTransformR Eval
    , listConsumer  :: C.EntryConsumerR Eval
    }

type ParseState = (Int, [String])


checkHelpArg :: [String] -> Bool
checkHelpArg args = "-h" `elem` args || "--help" `elem` args


parseFlags :: ParseState -> (ParseState, [FindFlag])
parseFlags (i, "-n":args) = fmap (DryRun:)      $ parseFlags (i+1, args)
parseFlags (i, "-L":args) = fmap (FollowLinks:) $ parseFlags (i+1, args)
parseFlags (i,      args) = ((i, args), [])


parsePathArgs :: ParseState
              -> IO (Either RawPath (ParseState, [Path Abs Dir]))
parsePathArgs state@(_, [])        = return (Right (state, []))
parsePathArgs state@(_, ('-':_):_) = return (Right (state, []))
parsePathArgs (i, arg:args) = do
    let rawPath = T.pack arg
    res <- Path.canonicalizeFromHere rawPath

    case res of
        Nothing   -> return (Left rawPath)
        Just path -> (fmap.fmap.fmap) (Path.asDirPath path:) $
                         parsePathArgs (i+1, args)


parseTreeTransforms :: ParseState
                    -> BakerT (Except String) (ParseState, C.TransformR Eval)
parseTreeTransforms (i, []) =
    return ((i, []), Pipes.cat)
parseTreeTransforms (i, "-prune":expr:args) = do
    let sourceName = "argument #" ++ show (i+1)
    predicate <- parsePrunePredicate sourceName expr

    (state', trns) <- parseTreeTransforms (i+2, args)
    return (state', C.pruneDirsM predicate >-> trns)

parseTreeTransforms state = return (state, Pipes.cat)


parseListTransforms :: ParseState
                    -> BakerT (Except String) (ParseState, C.EntryTransformR Eval)
parseListTransforms (i, []) =
    return ((i, []), Pipes.cat)
parseListTransforms (i, "-if":expr:args) = do
    let sourceName = "argument #" ++ show (i+1)
    predicate <- parseFilterPredicate sourceName expr

    (state', trns) <- parseListTransforms (i+2, args)
    return (state', Pipes.filterM predicate >-> trns)

parseListTransforms state = return (state, Pipes.cat)


parseConsumer :: ParseState
              -> BakerT (Except String) (ParseState, C.EntryConsumerR Eval)
parseConsumer (i, []) =
    return ( (i, [])
           , C.asPaths
               >-> C.asFiles
               >-> C.plainText
               >-> Pipes.mapM_ (liftIO . TextIO.putStrLn)
           )

parseConsumer (i, "-print":fmt:args) = do
    let sourceName = "argument #" ++ show (i+1)
    format <- parseStringInterp sourceName fmt

    (st', consumer) <- if null args
                         then return ((i+2, args), Pipes.drain)
                         else parseConsumer (i+2, args)
    return ( st'
           , Pipes.chain (liftIO . TextIO.putStrLn <=< format)
               >-> consumer
           )

parseConsumer (i, "-exec":args) = do
    let sources = zip ["argument #" ++ show j | j <- [i+1..]]
                      args
    cmd <- parseCmdLine sources

    return ( (i+1 + length args, [])
           , Pipes.mapM_ (liftIO <=< cmd)
           )

parseConsumer (i, "-nop":args)
  | null args = return ((i+1, args), Pipes.drain)
  | otherwise = parseConsumer (i+2, args)

parseConsumer (i, args) =
    return ((i, args), Pipes.drain)


parseArgs :: [String] -> ExceptT String IO [Result]
parseArgs args = do
    when (checkHelpArg args) $ liftIO $ do
        putStrLn usage
        exitSuccess

    state          <- return (1, args)
    (state, flags) <- return $ parseFlags state
    (state, paths) <- ExceptT $ fmap fmtIfBadPathError (parsePathArgs state)

    forM paths $ \path -> do
        ((state, treeTrns), ctx1) <- runBaker path $ parseTreeTransforms state
        ((state, listTrns), ctx2) <- runBaker path $ parseListTransforms state

        ((state, listCons), ctx2) <- runBakerWith ctx2 path $ parseConsumer state

        case snd state of
            []    -> return (Result flags path ctx1 treeTrns
                                               ctx2 listTrns
                                               listCons)

            arg:_ -> liftIO $ die ("Unrecognized argument: " ++ arg)
  where
    fmtIfBadPathError (Left rawPath) = Left ("Invalid path: " ++ show rawPath)
    fmtIfBadPathError (Right a)      = Right a

    tshow  = T.pack . show
    putErr = liftIO . TextIO.hPutStrLn stderr

    reportError ctx notFound bt = do
        let name = case notFound of
                       RxCapVar i -> T.pack (show i)
                       NamedVar n -> n

        putErr (Err.renderError
                 (Err.errorWithBacktrace
                   ("Variable $" <> name <> " not found")
                   bt))

        putErr ""
        putErr "Defined variables:"
        forM_ (Baker.ctxGetVars ctx) $ \var ->
            putErr ("    $" <> var)

        case Baker.ctxGetActiveRegex ctx of
            Nothing -> return ()
            Just rx -> putErr ("\nActive regex: " <> tshow (ICU.pattern rx))

    runBaker :: Path Abs Dir
             -> BakerT (Except String) a
             -> ExceptT String IO (a, Baker.BakingContext)
    runBaker = runBakerWith Baker.defBakingContext

    runBakerWith :: Baker.BakingContext
                 -> Path Abs Dir
                 -> BakerT (Except String) a
                 -> ExceptT String IO (a, Baker.BakingContext)
    runBakerWith ctx path baker = do
        (e, ctx) <- runBakerT path ctx $
                        hoist (hoist generalize) baker
        case e of
            Right res ->
                return (res, ctx)
            Left (Err.VarNotFound notFound, bt) -> do
                liftIO $ reportError ctx notFound bt
                throwE ""




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

