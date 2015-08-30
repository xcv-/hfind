{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module System.Posix.Find.Lang.Context
    ( VarId
    -- monads
    , Baker
    , BakerT
    , runBakerT, BakerResult
    , Eval
    , Evaluator(..), EvalResult
    -- Baker
    , BakerConfig(..)
    , bakeReadonly
    , newVar
    , getVarId
    , getOrNewVar
    , getNumCaptures
    , updateNumCaptures
    , lookupEnv
    , frame
    , getBacktrace
    -- Builtins
    , lookupBuiltinVar
    , lookupBuiltinFunc
    -- Eval
    , evalReadonly
    , getVarValue
    , setVarValue
    , getCaptureValue
    , setCaptures
    , evalWithin
    ) where

import Data.Functor.Identity (Identity)

import Control.Monad.Trans.Except (throwE, catchE)

import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow, MonadCatch, catchAll)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import Control.Monad.Morph

import Data.List (elemIndex)

import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import qualified Data.Text.ICU as ICU

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as V

import qualified System.Posix.Env.ByteString as Posix

import System.Posix.Text.Path (Path, Abs, Dir)

import System.Posix.Find.Lang.Types (Name, SrcLoc,
                                     Value(..), RxCaptureMode(..))

import qualified System.Posix.Find.Lang.Error    as Err
import qualified System.Posix.Find.Lang.Builtins as Builtins


newtype VarId = VarId Int

data BakingContext = BakingContext
    { ctxVars        :: ![Name] -- reversed
    , ctxNumVars     :: !Int
    , ctxActiveRegex :: !(Maybe ICU.Regex)
    , ctxBacktrace   :: !Err.Backtrace
    }

defBakingContext :: BakingContext
defBakingContext = BakingContext [] 0 Nothing Err.emptyBacktrace

data EvalContext = EvalContext
    { ctxValues      :: !(IOVector Value) -- reversed
    , ctxActiveMatch :: !(Maybe ICU.Match)
    , ctxRuntimeBt   :: !Err.Backtrace
    }

defEvalContext :: Int -> IO EvalContext
defEvalContext maxVals = do
    vals <- V.new maxVals
    return (EvalContext vals Nothing Err.emptyBacktrace)


-- BakerT/EvalT transformers -----------------------------------------

data BakerConfig = BakerConfig
    { bakerRoot     :: Path Abs Dir
    , bakerBuiltins :: Builtins.Builtins (EvalT IO)
    , bakerSysEnv   :: [(ByteString, Value)]
    }

type Baker a = BakerT Identity a

newtype BakerT m a =
    BakerT (ExceptT (Err.VarNotFoundError, Err.Backtrace)
             (StateT BakingContext
               (ReaderT BakerConfig m))
                 a)
    deriving (Functor, Applicative, Monad,
              MonadReader BakerConfig,
              MonadIO)

type Eval = EvalT IO

newtype EvalT m a =
    EvalT (ExceptT (Err.RuntimeError, Err.Backtrace)
            (StateT EvalContext m)
              a)
    deriving (Functor, Applicative, Monad,
              MonadIO, MonadThrow, MonadCatch)


instance Monad m => MonadError Err.VarNotFoundError (BakerT m) where
    throwError e = BakerT $ do
        bt <- gets ctxBacktrace
        throwE (e, bt)

    (BakerT ma) `catchError` h = BakerT $
        ma `catchE` \(e, _) -> let (BakerT m) = h e in m

instance Monad m => MonadError Err.RuntimeError (EvalT m) where
    throwError e = EvalT $ do
        bt <- gets ctxRuntimeBt
        throwE (e, bt)

    (EvalT ma) `catchError` h = EvalT $
        ma `catchE` \(e, _) -> let (EvalT m) = h e in m


instance MonadTrans BakerT where
    lift = BakerT . lift . lift . lift

instance MFunctor BakerT where
    hoist f (BakerT m) = BakerT $ hoist (hoist (hoist f)) m

instance MonadTrans EvalT where
    lift = EvalT . lift . lift

instance MFunctor EvalT where
    hoist f (EvalT m) = EvalT $ hoist (hoist f) m


type BuiltinVar  = Builtins.BuiltinVar  (EvalT IO)
type BuiltinFunc = Builtins.BuiltinFunc (EvalT IO)


type BakerResult a =
    ( Either (Err.VarNotFoundError, Err.Backtrace) (a, Evaluator)
    , [Name]
    , Maybe ICU.Regex
    )

type EvalResult a =
    ( Either (Err.RuntimeError, Err.Backtrace) a
    , [(Name, Value)]
    , Maybe ICU.Match
    )

newtype Evaluator = Evaluator
    { runEvalT :: forall m a. MonadIO m => EvalT m a -> m (EvalResult a) }


runBakerT :: MonadIO m => Path Abs Dir -> BakerT m a -> m (BakerResult a)
runBakerT root (BakerT m) = do
    env <- liftIO Posix.getEnvironment

    let cfg = BakerConfig {
          bakerRoot     = root
        , bakerBuiltins = Builtins.mkBuiltins root
        , bakerSysEnv   = map (\(k,v) -> (k, StringV (T.decodeUtf8 v))) env
        }

    (ma, s) <- runReaderT (runStateT (runExceptT m) defBakingContext) cfg

    let run = Evaluator $ \(EvalT n) -> do
                  ctx       <- liftIO $ defEvalContext (ctxNumVars s)
                  (a, ctx') <- runStateT (runExceptT n) ctx

                  vals <- liftIO $ V.toList <$> V.freeze (ctxValues ctx')

                  let varDump = zip (ctxVars s) vals
                  return (a, varDump, ctxActiveMatch ctx')

    return ( fmap (\a -> (a, run)) ma
           , ctxVars s
           , ctxActiveRegex s
           )


-- Baker primitives --------------------------------------------------

bakeReadonly :: Monad m => BakerT m a -> BakerT m a
bakeReadonly (BakerT m) = BakerT $ do
    s <- get
    a <- m
    put s
    return a


newVar :: Monad m => Name -> BakerT m VarId
newVar name = BakerT $ do
    varId <- gets (VarId . ctxNumVars)

    modify $ \ctx ->
      ctx { ctxVars       = ctxVars ctx ++ [name]
          , ctxNumVars    = 1 + ctxNumVars ctx
          }

    return varId

getVarId :: Monad m => Name -> BakerT m (Maybe VarId)
getVarId name = BakerT $ do
    vars  <- gets ctxVars

    return $ VarId <$> elemIndex name vars

getOrNewVar :: Monad m => Name -> BakerT m VarId
getOrNewVar name = do
    var <- getVarId name
    case var of
        Just i  -> return i
        Nothing -> newVar name


getNumCaptures :: Monad m => BakerT m Int
getNumCaptures = BakerT $ do
    activeRx <- gets ctxActiveRegex

    case activeRx of
        -- +1 because the full pattern is $0
        Just rx -> return (1 + ICU.groupCount rx)
        Nothing -> return 0

updateNumCaptures :: Monad m => RxCaptureMode -> ICU.Regex -> BakerT m ()
updateNumCaptures NoCapture _  = return ()
updateNumCaptures Capture   rx = BakerT $
    modify $ \ctx -> ctx { ctxActiveRegex = Just rx }


lookupEnv :: Monad m => ByteString -> BakerT m (Maybe Value)
lookupEnv name = BakerT $ do
    env <- asks bakerSysEnv
    return $! lookup name env

lookupBuiltinVar :: Monad m => Name -> BakerT m (Maybe BuiltinVar)
lookupBuiltinVar name = BakerT $ do
    builtins <- asks bakerBuiltins
    return $! Builtins.lookupVar builtins name

lookupBuiltinFunc :: Monad m => Name -> BakerT m (Maybe BuiltinFunc)
lookupBuiltinFunc name = BakerT $ do
    builtins <- asks bakerBuiltins
    return $! Builtins.lookupFunc builtins name


frame :: Monad m => Text -> SrcLoc -> BakerT m a -> BakerT m a
frame name (src, loc) (BakerT ma) = BakerT $ do
    modify $ \ctx -> ctx {
        ctxBacktrace = Err.pushFrame (Err.BtFrame name src loc)
                                     (ctxBacktrace ctx)
    }

    a <- ma

    modify $ \ctx -> ctx {
        ctxBacktrace = Err.popFrame (ctxBacktrace ctx)
    }

    return a

getBacktrace :: Monad m => BakerT m Err.Backtrace
getBacktrace = BakerT $ gets ctxBacktrace


-- Eval primitives --------------------------------------------------

evalReadonly :: MonadIO m => EvalT m a -> EvalT m a
evalReadonly (EvalT m) = EvalT $ do
    -- variables may change in the action, but the current language does not
    -- allow it in predicates and this is just used in Eval.hs, so we're
    -- safe for now
    s <- get
    a <- m
    put s
    return a


getVarValue :: (MonadIO m, MonadCatch m) => VarId -> EvalT m Value
getVarValue (VarId i) = do
    vals <- EvalT $ gets ctxValues

    do (!val) <- liftIO (V.read vals i)
       return val
    `catchAll` (throwError . Err.NativeError)


setVarValue :: (MonadIO m, MonadCatch m) => VarId -> Value -> EvalT m ()
setVarValue (VarId i) val = do
    vals <- EvalT $ gets ctxValues

    liftIO (V.write vals i val)
    `catchAll` (throwError . Err.NativeError)


getCaptureValue :: Monad m => Int -> EvalT m Text
getCaptureValue i = EvalT $ do
    Just match <- gets ctxActiveMatch

    case ICU.group i match of
        Just text -> return text
        Nothing ->
            error $ "getCaptureValue: could not find $" ++ show i
                      ++ " of " ++ T.unpack (ICU.pattern match)


setCaptures :: Monad m => RxCaptureMode -> ICU.Match -> EvalT m ()
setCaptures NoCapture _     = return ()
setCaptures Capture   match = EvalT $
    modify $ \ctx -> ctx { ctxActiveMatch = Just match }


evalWithin :: Monad m => Err.Backtrace -> EvalT m a -> EvalT m a
evalWithin bt (EvalT ma) = EvalT $ do
    prev <- get
    modify $ \ctx -> ctx { ctxRuntimeBt = bt }
    a <- ma
    put prev
    return a
