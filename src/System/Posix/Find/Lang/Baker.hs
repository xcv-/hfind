{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module System.Posix.Find.Lang.Baker
    ( VarId, varIdNum
    , Baker
    , BakerT
    , runBakerT, BakerResult
    , EvalMonad
    , BakerConfig(..)
    , BakingContext
    , defBakingContext
    , ctxGetVars, ctxGetNumVars, ctxGetActiveRegex, ctxGetBacktrace
    , readonly
    , newVar
    , getVarId
    , getOrNewVar
    , getNumCaptures
    , updateNumCaptures
    , lookupEnv
    , frame
    , getBacktrace
    , lookupBuiltinVar
    , lookupBuiltinFunc
    ) where

import Data.Functor.Identity (Identity)

import Control.Monad.Trans.Except (throwE, catchE)

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import Control.Monad.Morph

import Data.List (elemIndex)

import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text.Encoding as T

import qualified Data.Text.ICU as ICU

import qualified System.Posix.Env.ByteString as Posix

import System.Posix.Text.Path (Path, Abs, Dir)

import System.Posix.Find.Lang.Types (Name, SrcLoc,
                                     Value(..), RxCaptureMode(..))

import qualified System.Posix.Find.Lang.Error    as Err
import qualified System.Posix.Find.Lang.Builtins as Builtins


-- trick to avoid cyclic package dependencies
type family EvalMonad (baker :: (* -> *) -> * -> *) :: * -> *


newtype VarId = VarId Int

varIdNum :: VarId -> Int
varIdNum (VarId i) = i


data BakingContext = BakingContext
    { ctxVars        :: ![Name] -- reversed
    , ctxNumVars     :: !Int
    , ctxActiveRegex :: !(Maybe ICU.Regex)
    , ctxBacktrace   :: !Err.Backtrace
    }

defBakingContext :: BakingContext
defBakingContext = BakingContext [] 0 Nothing Err.emptyBacktrace

ctxGetVars :: BakingContext -> [Name]
ctxGetVars = ctxVars

ctxGetNumVars :: BakingContext -> Int
ctxGetNumVars = ctxNumVars

ctxGetActiveRegex :: BakingContext -> Maybe ICU.Regex
ctxGetActiveRegex = ctxActiveRegex

ctxGetBacktrace :: BakingContext -> Err.Backtrace
ctxGetBacktrace = ctxBacktrace


-- BakerT transformers -----------------------------------------

data BakerConfig = BakerConfig
    { bakerRoot     :: Path Abs Dir
    , bakerBuiltins :: Builtins.Builtins (EvalMonad BakerT)
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


instance Monad m => MonadError Err.VarNotFoundError (BakerT m) where
    throwError e = BakerT $ do
        bt <- gets ctxBacktrace
        throwE (e, bt)

    (BakerT ma) `catchError` h = BakerT $
        ma `catchE` \(e, _) -> let (BakerT m) = h e in m

instance MonadTrans BakerT where
    lift = BakerT . lift . lift . lift

instance MFunctor BakerT where
    hoist f (BakerT m) = BakerT $ hoist (hoist (hoist f)) m



type BuiltinVar  = Builtins.BuiltinVar  (EvalMonad BakerT)
type BuiltinFunc = Builtins.BuiltinFunc (EvalMonad BakerT)


type BakerResult a =
    ( Either (Err.VarNotFoundError, Err.Backtrace) a
    , BakingContext
    )


runBakerT :: ( MonadIO m
             , MonadIO (EvalMonad BakerT)
             , MonadError Err.RuntimeError (EvalMonad BakerT)
             )
          => Path Abs Dir
          -> BakingContext
          -> BakerT m a
          -> m (BakerResult a)
runBakerT root ctx (BakerT m) = do
    env <- liftIO Posix.getEnvironment

    let cfg = BakerConfig {
          bakerRoot     = root
        , bakerBuiltins = Builtins.mkBuiltins root
        , bakerSysEnv   = map (\(k,v) -> (k, StringV (T.decodeUtf8 v))) env
        }

    runReaderT (runStateT (runExceptT m) ctx) cfg



readonly :: Monad m => BakerT m a -> BakerT m a
readonly (BakerT m) = BakerT $ do
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


