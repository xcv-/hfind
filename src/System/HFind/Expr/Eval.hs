{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module System.HFind.Expr.Eval
    ( Eval
    , EvalContext
    , ctxGetValues, ctxGetActiveMatch, ctxGetBacktrace, ctxGetBakerContext
    , newContext
    , runEvalT
    , wrapEvalT
    , readonly
    , getVarValue
    , setVarValue
    , getCaptureValue
    , setCaptures
    , evalWithin
    ) where

import Control.Monad.Trans.Except (throwE)

import Control.Monad.Catch (MonadThrow, MonadCatch, catchAll)
import Control.Monad.Except
import Control.Monad.Reader

import Control.Monad.Morph

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Text.ICU as ICU

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as V

import System.HFind.Expr.Types (Value(..), RxCaptureMode(..))

import System.HFind.Expr.Baker (VarId)
import qualified System.HFind.Expr.Baker as Baker
import qualified System.HFind.Expr.Error as Err


data EvalContext = EvalContext
    { ctxValues       :: IOVector Value -- reversed
    , ctxActiveMatch  :: IORef (Maybe ICU.Match)
    , ctxBacktrace    :: IORef Err.Backtrace
    , ctxBakerContext :: Baker.BakingContext
    }

newContext :: Baker.BakingContext -> IO EvalContext
newContext ctx = do
    vals        <- V.new (Baker.ctxGetNumVars ctx)
    activeMatch <- newIORef Nothing
    backtrace   <- newIORef Err.emptyBacktrace

    return (EvalContext vals activeMatch backtrace ctx)


ctxGetValues :: EvalContext -> IO [Value]
ctxGetValues ctx = do
    values <- V.freeze (ctxValues ctx)
    return (V.toList values)

ctxGetActiveMatch :: EvalContext -> IO (Maybe ICU.Match)
ctxGetActiveMatch = readIORef . ctxActiveMatch

ctxGetBacktrace :: EvalContext -> IO Err.Backtrace
ctxGetBacktrace = readIORef . ctxBacktrace

ctxGetBakerContext :: EvalContext -> Baker.BakingContext
ctxGetBakerContext = ctxBakerContext



type Eval = EvalT IO

type instance Baker.EvalMonad Baker.BakerT = Eval

newtype EvalT m a =
    EvalT (ExceptT Err.RuntimeError
            (ReaderT EvalContext m)
              a)
    deriving (Functor, Applicative, Monad,
              MonadIO,
              MonadError Err.RuntimeError,
              MonadThrow, MonadCatch)


instance MonadTrans EvalT where
    lift = EvalT . lift . lift

instance MFunctor EvalT where
    hoist f (EvalT m) = EvalT $ hoist (hoist f) m



-- Eval primitives --------------------------------------------------


runEvalT :: MonadIO m
         => EvalContext
         -> EvalT m a
         -> ExceptT (EvalContext, Err.RuntimeError) m a
runEvalT ctx (EvalT m) = do
    res <- lift $ runReaderT (runExceptT m) ctx
    case res of
        Left err -> throwE (ctx, err)
        Right a  -> return a


wrapEvalT :: Monad m
          => ExceptT (EvalContext, Err.RuntimeError) m a
          -> EvalT m a
wrapEvalT m = EvalT $ ExceptT $ ReaderT $ \_ -> do
    r <- runExceptT m
    case r of
        Left (_, e) -> return (Left e)
        Right a     -> return (Right a)



readonly :: MonadIO m => EvalT m a -> EvalT m a
readonly (EvalT m) = EvalT $ do
    -- variables may change in the action, but the current language does not
    -- allow it in predicates and this is just used in Eval.hs, so we're
    -- safe for now
    ctx <- ask

    match <- liftIO $ readIORef (ctxActiveMatch ctx)
    bt    <- liftIO $ readIORef (ctxBacktrace ctx)

    a <- m

    liftIO $ writeIORef (ctxActiveMatch ctx) match
    liftIO $ writeIORef (ctxBacktrace   ctx) bt

    return a


getVarValue :: (MonadIO m, MonadCatch m) => VarId -> EvalT m Value
getVarValue i = do
    vals <- EvalT $ asks ctxValues

    liftIO (V.read vals (Baker.varIdNum i))
    `catchAll` (throwError . Err.NativeError)


setVarValue :: (MonadIO m, MonadCatch m) => VarId -> Value -> EvalT m ()
setVarValue i val = do
    vals <- EvalT $ asks ctxValues

    liftIO (V.write vals (Baker.varIdNum i) val)
    `catchAll` (throwError . Err.NativeError)


getCaptureValue :: MonadIO m => Int -> EvalT m Text
getCaptureValue i = EvalT $ do
    Just match <- liftIO . readIORef =<< asks ctxActiveMatch

    case ICU.group i match of
        Just text -> return text
        Nothing ->
            error $ "getCaptureValue: could not find $" ++ show i
                      ++ " of " ++ T.unpack (ICU.pattern match)


setCaptures :: MonadIO m => RxCaptureMode -> ICU.Match -> EvalT m ()
setCaptures NoCapture _     = return ()
setCaptures Capture   match = EvalT $ do
    activeMatch <- asks ctxActiveMatch

    liftIO $ writeIORef activeMatch $! Just $! match


evalWithin :: MonadIO m => Err.Backtrace -> EvalT m a -> EvalT m a
evalWithin bt (EvalT ma) = EvalT $ do
    btRef <- asks ctxBacktrace
    prev <- liftIO $ readIORef btRef

    liftIO $ writeIORef btRef bt
    a <- ma
    liftIO $ writeIORef btRef prev

    return a

