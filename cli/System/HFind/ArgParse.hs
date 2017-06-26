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

import System.HFind.Expr.Types (TypedName(..), Var(..), typeName)

import System.HFind.Expr.Baker (BakerT, runBakerT)
import System.HFind.Expr.Eval  (Eval)

import qualified System.HFind.Expr.Baker as Baker

import System.HFind.Expr.Bakers (parseLetBinding,
                                 parseFilterPredicate,
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
parseListTransforms (i, "-let":expr:args) = do
    let sourceName = "argument #" ++ show (i+1)
    update <- parseLetBinding sourceName expr

    (state', trns) <- parseListTransforms (i+2, args)
    return (state', Pipes.chain update >-> trns)
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

    putErr = liftIO . TextIO.hPutStrLn stderr

    fmtError :: Err.BakingError -> T.Text
    fmtError (Err.VarNotFound notFound) =
        let name = case notFound of
                       RxCapVar i -> T.pack (show i)
                       NamedVar n -> n
        in
           "Variable $" <> name <> " not found"
    fmtError (Err.VarAlreadyExists name) =
        "Variable $" <> name <> " is already in scope"
    fmtError (Err.ExpectedButFound t1 t2) =
        "Type error: '" <> t1 <> "' expected, but found "
                 <> "'" <> t2 <> "'"

    reportError ctx err bt = do
        putErr (Err.renderError
                 (Err.errorWithBacktrace (fmtError err) bt))

        putErr ""
        putErr "Defined variables:"
        forM_ (Baker.ctxGetVars ctx) $ \(TypedName ty var) ->
            putErr ("    $" <> var <> ": " <> typeName ty)

        case Baker.ctxGetActiveRegex ctx of
            Nothing -> return ()
            Just rx -> putErr ("\nActive regex: " <> ICU.pattern rx)

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
            Left (err, bt) -> do
                liftIO $ reportError ctx err bt
                throwE ""




usage :: String
usage = [r|usage: hfind [-L] <path> [tree transforms] [list transforms] [consumers]

flags:
  -L                 follow symlinks

supported tree transforms:
  -prune pred:       filter subtrees with root matching pred

supported list transforms:
  -if pred           allow only entries satisfying the predicate pred
  -let var=expr      define a new variable with value expr

supported consumers:
  -print string      (default) print the string, followed by a new line
  -exec cmd a1 a2... execute a shell command cmd with arguments a1, a2,...
                       where cmd and a1, a2,... are parsed as individual string
                       interpolations without quotes
  -nop               do nothing

predicate syntax:
  scope(pred)   nests a scope, ensuring that regex captures in pred are not
                reflected outside

  expr (of boolean type)

  not(pred)    pred1 and pred2     pred1 or pred2

  pred1 == pred2        pred1 != pred2
  pred1 >= pred2        pred1 <= pred2
  pred1 >  pred2        pred1 <  pred2

  expr =~ m/pcre regex/mxsin

  valid regex delimiters:
    / _ @ % # ! , ; |

  regex options:
    m: multiline               s: dot matches newlines
    x: allow spaces/comments   i: case insensitive
    n: do not capture

expression syntax:
  $variable
  literal
  function expr1
  "interpolated string with $variables, dollars escaped as $$"

  +expr                 -expr
  expr1 + expr 2        expr1 - expr2
  expr1 * expr 2        expr1 / expr2

types:
  string, int (64-bit), bool, fsnode (file metadata)

variable syntax:
  $identifier
  $builtin_function (applies builtin_function to the current node)
  $environment_variable
  regex captures: $0, $1, $2...

builtin functions:
  readint : string -> int
  exists, isfile, isdir, islink : string -> bool
  stat : string -> fsnode

  parent : fsnode -> fsnode
  hidden : fsnode -> bool
  type   : fsnode -> string
  size   : fsnode -> int
  nlinks : fsnode -> int
  perms  : fsnode -> string (e.g. rwxr-xr-x)
  owner, group : fsnode -> string
  ownerid, groupid : fsnode -> string
  name, path, relpath, parentpath, parentname : fsnode -> string


literal syntax:
  "string"
  12312319283791283 (64-bit integer)
  true
  false |]

