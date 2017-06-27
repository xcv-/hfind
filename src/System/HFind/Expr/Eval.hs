{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module System.HFind.Expr.Eval
    ( Eval
    , EvalContext
    , UninitializedVariableException(..)
    , RuntimeVar(..)
    , ctxGetVarValues, ctxGetActiveMatch, ctxGetBacktrace, ctxGetBakerContext
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

import Control.Exception (Exception, throw)

import Control.Monad.Trans.Except (throwE)

import Control.Monad.Catch (MonadThrow, MonadCatch, catchAll)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Morph

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Data.Text (Text)
import qualified Data.Text     as T
import qualified Data.Text.ICU as ICU

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

import Data.Void (Void)

import Unsafe.Coerce (unsafeCoerce)

import System.HFind.Expr.Types (Name, TypedName(..),
                                ErasedValue(..), Value,
                                eraseType, RxCaptureMode(..))

import System.HFind.Expr.Baker (VarId)
import qualified System.HFind.Expr.Baker as Baker
import qualified System.HFind.Expr.Error as Err


data EvalContext = EvalContext
    { ctxValues       :: !(IOVector (Value Void)) -- reversed
    , ctxActiveMatch  :: !(IORef (Maybe ICU.Match))
    , ctxBacktrace    :: !(IORef Err.Backtrace)
    , ctxBakerContext :: !Baker.BakingContext
    }

instance Show EvalContext where
    show _ = "<ctx>"

data UninitializedVariableException = UninitializedVariableException
    deriving (Eq, Show)
instance Exception UninitializedVariableException

newContext :: Baker.BakingContext -> IO EvalContext
newContext ctx = do
    vals        <- MV.replicate (Baker.ctxGetNumVars ctx)
                                (throw UninitializedVariableException)
    activeMatch <- newIORef Nothing
    backtrace   <- newIORef Err.emptyBacktrace

    return (EvalContext vals activeMatch backtrace ctx)


data RuntimeVar where
    RuntimeVar :: !Name -> !(Value t) -> RuntimeVar

ctxGetVarValues :: EvalContext -> IO [RuntimeVar]
ctxGetVarValues ctx = do
    let vars = Baker.ctxGetVars (ctxBakerContext ctx)
    values <- V.freeze (ctxValues ctx)
    return (zipWith runtimeVar vars (V.toList values))
  where
    runtimeVar :: TypedName -> Value Void -> RuntimeVar
    runtimeVar (TypedName ty name) val =
        case eraseType ty (unsafeCoerce val) of
            ErasedValue val' -> RuntimeVar name val'

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


-- evil
unsafeCoerceValue :: VarId a -> Value Void -> Value a
unsafeCoerceValue _ = unsafeCoerce

unsafeVoidValue :: VarId a -> Value a -> Value Void
unsafeVoidValue _ = unsafeCoerce


getVarValue :: (MonadIO m, MonadCatch m) => VarId a -> EvalT m (Value a)
getVarValue i = do
    vals <- EvalT $ asks ctxValues

    val <- liftIO (MV.read vals (Baker.varIdNum i))
             `catchAll` (throwError . Err.NativeError)

    return (unsafeCoerceValue i val)


setVarValue :: (MonadIO m, MonadCatch m) => VarId a -> Value a -> EvalT m ()
setVarValue i val = do
    vals <- EvalT $ asks ctxValues

    liftIO (MV.write vals (Baker.varIdNum i) (unsafeVoidValue i val))
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

