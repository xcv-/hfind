{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module System.HFind.ArgParse
  ( FindFlag(..)
  , Result(..)
  , getNumThreads
  , parseArgs
  ) where

import qualified GHC.Conc as GHC

import Control.Monad              (when, forM, forM_, (<=<))
import Control.Monad.Trans.Except (ExceptT(..), Except, throwE)
import Control.Monad.Morph        (hoist, generalize)
import Control.Monad.IO.Class     (liftIO)

import Data.List (break, isPrefixOf)
import Data.Monoid ((<>))
import Text.Read (readMaybe)

import qualified Data.Text     as T
import qualified Data.Text.IO  as TextIO
import qualified Data.Text.ICU as ICU

import System.Exit (die, exitSuccess)
import System.IO   (stderr)

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
                                 parseExecCmdLine)

import qualified System.HFind.Expr.Error as Err


data FindFlag = FollowLinks | DryRun | Parallel (Maybe Int)
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

getNumThreads :: Result -> IO Int
getNumThreads result =
    case [ n | Parallel n <- findFlags result ] of
        []        -> return 1
        Nothing:_ -> GHC.getNumProcessors
        Just n:_  -> return n

type ParseState = (Int, [String])


checkHelpArg :: [String] -> Bool
checkHelpArg []      = False
checkHelpArg (arg:_) = arg == "-h" || arg == "--help"


parseFlags :: ParseState -> (ParseState, [FindFlag])
parseFlags (i, "-n":args) = fmap (DryRun:)           $ parseFlags (i+1, args)
parseFlags (i, "-L":args) = fmap (FollowLinks:)      $ parseFlags (i+1, args)
parseFlags (i, "-j":args) = fmap (Parallel Nothing:) $ parseFlags (i+1, args)
parseFlags (i,   jN:args)
  | "-j" `isPrefixOf` jN
  , Just n <- readMaybe (drop 2 jN)
  = fmap (Parallel (Just n):) $ parseFlags (i+1, args)
parseFlags (i, args) = ((i, args), [])


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



parseConsumers :: ParseState
               -> BakerT (Except String) (ParseState, C.EntryConsumerR Eval)
parseConsumers (i, []) =
    return ( (i, [])
           , C.asPaths
               >-> C.asFiles
               >-> C.plainText
               >-> Pipes.mapM_ (liftIO . TextIO.putStrLn)
           )
parseConsumers (i, "-print":fmt:args) = do
    let sourceName = "argument #" ++ show (i+1)
    format <- parseStringInterp sourceName fmt
    let consume = liftIO . TextIO.putStrLn <=< format

    (state', consumer) <- parseMoreConsumers (i+2, args)
    return (state', Pipes.chain consume >-> consumer)

parseConsumers (i, arg:args) | arg == "-exec" || arg == "-execdir" = do
      let (argv,   args')  = break (==";") args
          (offset, args'') = case args' of
                                 ";":args'' -> (1, args'')
                                 _          -> (0, args')

      let sources = zip ["argument #" ++ show j | j <- [i+1..]] argv
      exec <- parseExecCmdLine ("argument #" ++ show i) (arg == "-execdir") sources

      (state', consumer) <- parseMoreConsumers (i+1 + offset + length argv, args'')
      return (state', Pipes.chain exec >-> consumer)

parseConsumers (i, "-nop":args) = parseMoreConsumers (i+1, args)

parseConsumers state = return (state, Pipes.drain)


parseMoreConsumers :: ParseState
                   -> BakerT (Except String) (ParseState, C.EntryConsumerR Eval)
parseMoreConsumers st@(_, []) = return (st, Pipes.drain)
parseMoreConsumers st         = parseConsumers st



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

        ((state, listCons), ctx2) <- runBakerWith ctx2 path $ parseConsumers state

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
    fmtError (Err.FuncNotFound name) =
        "No function called " <> name <> " exists"
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
  -L                   Follow symlinks
  -jN                  Use one producer and (N-1) consumer threads. Particularly
                       appropriate with -exec. If N is ommited, it defaults to
                       the number of available processors.

supported tree transforms:
  -prune pred:         Filter subtrees with root matching pred

supported list transforms:
  -if pred             Allow only entries satisfying the predicate pred
  -let var=expr        Define a new variable with value expr

supported consumers:
  -print string        Print the string, followed by a new line (default)
  -exec cmd a1... \;   Execute a shell command cmd with arguments a1, a2,...
                       where cmd and a1, a2,... are parsed as individual string
                       interpolations without quotes. The semicolon is optional
                       and required only if this is not the last argument.
  -execdir             Same as -exec, but set the working directory to the
                       directory containing each item.
  -nop                 Do nothing

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

