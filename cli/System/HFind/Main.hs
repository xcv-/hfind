{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module System.HFind.Main (main) where

import Data.Monoid ((<>))
import Data.Bifoldable
import Data.Typeable (cast)

import Data.Text (Text)
import qualified Data.Text     as T
import qualified Data.Text.IO  as TextIO
import qualified Data.Text.ICU as ICU

import Control.Monad              (forM_)
import Control.Monad.Morph        (hoist)
import Control.Monad.Trans.Except (ExceptT, runExceptT)

import qualified Control.Exception as Ex

import System.Environment (getArgs)
import System.Exit        (exitFailure, die)
import System.IO          (stderr)

import Pipes (Pipe, Consumer, (>->))
import qualified Pipes         as Pipes
import qualified Pipes.Prelude as Pipes

import qualified System.HFind.Path as Path

import System.HFind.Types

import qualified System.HFind.Walk        as Walk
import qualified System.HFind.Combinators as C

import System.HFind.Expr.Types (toValue, valueToString)

import System.HFind.Expr.Eval (Eval, runEvalT, wrapEvalT, UninitializedVariableException(..))
import qualified System.HFind.Expr.Baker as Baker
import qualified System.HFind.Expr.Eval  as Eval
import qualified System.HFind.Expr.Error as Err

import qualified System.HFind.ArgParse as ArgParse



type M = ExceptT (Eval.EvalContext, Err.RuntimeError) IO


hoistTreeEval :: Eval.EvalContext
              -> Pipe (WalkR Eval) (WalkR Eval) Eval ()
              -> Pipe (WalkR M)    (WalkR M)    M    ()
hoistTreeEval ctx pipe =
        Pipes.map (C.hoistWalk wrapEvalT)
    >-> hoist (runEvalT ctx) pipe
    >-> Pipes.map (C.hoistWalk (runEvalT ctx))


hoistListEval :: Eval.EvalContext
              -> Pipe a b Eval ()
              -> Pipe a b M    ()
hoistListEval ctx pipe =
    hoist (runEvalT ctx) pipe


buildPipeline :: ArgParse.Result -> IO (Consumer (WalkN' M) M ())
buildPipeline parseResult = do
    (Right treeCur, treeCtx) <- Baker.runBakerT (ArgParse.rootPath parseResult)
                                                (ArgParse.treeContext parseResult)
                                                (Baker.newVar "_current")
    (Right listCur, listCtx) <- Baker.runBakerT (ArgParse.rootPath parseResult)
                                                (ArgParse.listContext parseResult)
                                                (Baker.newVar "_current")

    treeEvalCtx <- Eval.newContext treeCtx
    listEvalCtx <- Eval.newContext listCtx

    return $ C.onError C.report
         >-> hoistTreeEval treeEvalCtx (treeTransform treeCur)
         >-> C.flatten
         >-> hoistListEval listEvalCtx (listTransform listCur)
         >-> hoistListEval listEvalCtx (listConsumer  listCur)
  where
    setCurrentNode :: Baker.VarId Text -> FSNode t 'Resolved -> Eval ()
    setCurrentNode var n =
        Eval.setVarValue var (toValue (Path.toText (nodePath n)))

    treeTransform :: Baker.VarId Text -> Pipe (WalkR Eval) (WalkR Eval) Eval ()
    treeTransform curNode =
        Pipes.mapM
          (C.bimapM_ (setCurrentNode curNode) (setCurrentNode curNode))
        >-> ArgParse.treeTransform parseResult

    listTransform :: Baker.VarId Text -> Pipe NodeListEntryR NodeListEntryR Eval ()
    listTransform curNode =
        Pipes.chain
          (bitraverse_ (setCurrentNode curNode) (setCurrentNode curNode))
        >-> ArgParse.listTransform parseResult

    listConsumer :: Baker.VarId Text -> Consumer NodeListEntryR Eval ()
    listConsumer _ =
        -- already set in listTransform
        ArgParse.listConsumer parseResult


printEvalError :: [Eval.RuntimeVar]
               -> Maybe ICU.Match
               -> (Err.RuntimeError, Err.Backtrace)
               -> IO ()
printEvalError varDump activeMatch (err, bt) = do
    let putErr = TextIO.hPutStrLn stderr

    putErr (Err.renderError (Err.errorWithBacktrace errMsg bt))

    putErr ""
    putErr "Variable dump:"

    forM_ varDump $ \(Eval.RuntimeVar var val) ->
        putErr ("    $" <> var <> " = " <> tshow (valueToString val))
        `Ex.catch` \(Ex.SomeException e) -> do
            case cast e of
                Just UninitializedVariableException ->
                    putErr ("    $" <> var <> " = <uninitialised>")
                Nothing ->
                    putErr ("    $" <> var <> " = <error> " <> tshow e)

    case activeMatch of
        Nothing    -> return ()
        Just match -> do
            putErr ""
            putErr ("Active match: " <> ICU.pattern match)

            forM_ [0 .. ICU.groupCount match] $ \i ->
                putErr ("    $" <> tshow i <> " = "
                                <> maybe "<error>" tshow (ICU.group i match))
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show

    errMsg :: Text
    errMsg =
        case err of
            Err.PrimError msg ->
                "Fatal error: " <> msg
            Err.NotFound path ->
                "File/Directory not found: '" <> path <> "'"
            Err.InvalidPathOp path op ->
                "Invalid path operation: trying to perform '"
                    <> op <> "' on '" <> path <> "'"
            Err.NativeError e ->
                "Native exception raised during evaluation: " <> tshow e



runFind :: ArgParse.Result -> IO ()
runFind parseResult = do
    let path   = ArgParse.rootPath parseResult
        follow = ArgParse.FollowLinks `elem` ArgParse.findFlags parseResult

    let following = Walk.walk Walk.followSymlinks   path >-> C.followLinks
        symlinks  = Walk.walk Walk.symlinksAreFiles path

    let walk = if follow then following else symlinks

    pipe <- buildPipeline parseResult

    res <- runExceptT $ Pipes.runEffect $ walk >-> pipe

    case res of
        Right () ->
            return ()

        Left (ctx, err) -> do
            bt          <- Eval.ctxGetBacktrace ctx
            vars        <- Eval.ctxGetVarValues ctx
            activeMatch <- Eval.ctxGetActiveMatch ctx
            printEvalError vars activeMatch (err, bt)



main :: IO ()
main = do
    parseResults <- getArgs >>= runExceptT . ArgParse.parseArgs >>= \case
        Right res -> return res
        Left err | null err  -> exitFailure
                 | otherwise -> die $ "error parsing arguments: " ++ err

    mapM_ runFind parseResults
