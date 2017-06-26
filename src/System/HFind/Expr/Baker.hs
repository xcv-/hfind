{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module System.HFind.Expr.Baker
    ( VarId, varIdNum
    , SomeVarId(..)
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
    , getNumCaptures
    , updateNumCaptures
    , lookupEnv
    , frame
    , getBacktrace
    , lookupBuiltinVar
    , lookupBuiltinFunc
    ) where


import Control.Monad.Trans.Except (throwE, catchE)

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import Control.Monad.Morph

import Data.ByteString (ByteString)

import Data.Functor.Identity (Identity)

import Data.Int (Int64)

import Data.List (find)

import Data.Text (Text)
import qualified Data.Text.Encoding as T

import qualified Data.Text.ICU as ICU

import qualified System.Posix.Env.ByteString as Posix

import System.HFind.Path (Path, Abs, Dir)

import System.HFind.Types (FSAnyNodeR)

import System.HFind.Expr.Types (Name, TypedName(..), SrcLoc(..),
                                IsValue(..), Value(..), ValueType(..),
                                RxCaptureMode(..))

import qualified System.HFind.Expr.Error    as Err
import qualified System.HFind.Expr.Builtins as Builtins


-- trick to avoid cyclic package dependencies
type family EvalMonad (baker :: (* -> *) -> * -> *) :: * -> *

newtype VarId t = VarId Int

data SomeVarId = forall t. IsValue t => SomeVarId !(VarId t)

varIdNum :: VarId t -> Int
varIdNum (VarId i) = i


data BakingContext = BakingContext
    { ctxVars        :: ![TypedName] -- reversed
    , ctxNumVars     :: !Int
    , ctxActiveRegex :: !(Maybe ICU.Regex)
    , ctxBacktrace   :: !Err.Backtrace
    }

defBakingContext :: BakingContext
defBakingContext = BakingContext [] 0 Nothing Err.emptyBacktrace

ctxGetVars :: BakingContext -> [TypedName]
ctxGetVars = ctxVars

ctxGetNumVars :: BakingContext -> Int
ctxGetNumVars = ctxNumVars

ctxGetActiveRegex :: BakingContext -> Maybe ICU.Regex
ctxGetActiveRegex = ctxActiveRegex

ctxGetBacktrace :: BakingContext -> Err.Backtrace
ctxGetBacktrace = ctxBacktrace


-- BakerT transformers -----------------------------------------

data BakerConfig = BakerConfig
    { bakerRoot     :: !(Path Abs Dir)
    , bakerBuiltins :: !(Builtins.Builtins (EvalMonad BakerT))
    , bakerSysEnv   :: ![(ByteString, Value Text)]
    }

type Baker a = BakerT Identity a

newtype BakerT m a =
    BakerT (ExceptT (Err.BakingError, Err.Backtrace)
             (StateT BakingContext
               (ReaderT BakerConfig m))
                 a)
    deriving (Functor, Applicative, Monad,
              MonadReader BakerConfig,
              MonadIO)


instance Monad m => MonadError Err.BakingError (BakerT m) where
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
    ( Either (Err.BakingError, Err.Backtrace) a
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
        , bakerSysEnv   = map (\(k,v) -> (k, toValue (T.decodeUtf8 v))) env
        }

    runReaderT (runStateT (runExceptT m) ctx) cfg



readonly :: Monad m => BakerT m a -> BakerT m a
readonly (BakerT m) = BakerT $ do
    s <- get
    a <- m
    put s
    return a


lookupVar :: Monad m => Name -> BakerT m (Maybe (Int, TypedName))
lookupVar name = BakerT $ do
    ixvars <- gets (zip [0..] . ctxVars)

    return (find (\(_, TypedName _ name') -> name == name') ixvars)

unsafeNewVar :: (IsValue t, Monad m) => Name -> BakerT m (VarId t)
unsafeNewVar name = BakerT $ do
    varId <- gets (VarId . ctxNumVars)

    let vt = valueTypeOf varId -- proxy ~ VarId

    modify $ \ctx ->
      ctx { ctxVars       = ctxVars ctx ++ [TypedName vt name]
          , ctxNumVars    = 1 + ctxNumVars ctx
          }

    return varId

dynVarId :: Int -> ValueType -> SomeVarId
dynVarId i TBool   = SomeVarId (VarId @Bool i)
dynVarId i TNum    = SomeVarId (VarId @Int64 i)
dynVarId i TString = SomeVarId (VarId @Text i)
dynVarId i TNode   = SomeVarId (VarId @FSAnyNodeR i)


newVar :: (IsValue t, Monad m) => Name -> BakerT m (VarId t)
newVar name = do
    existing <- lookupVar name

    case existing of
        Nothing -> unsafeNewVar name
        Just _  -> throwError $ Err.VarAlreadyExists name

getVarId :: Monad m => Name -> BakerT m (Maybe SomeVarId)
getVarId name = do
    existing <- lookupVar name

    case existing of
        Just (i, TypedName vt _) -> return (Just (dynVarId i vt))
        Nothing                  -> return Nothing

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


lookupEnv :: Monad m => ByteString -> BakerT m (Maybe (Value Text))
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
frame name (SrcLoc src loc) (BakerT ma) = BakerT $ do
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
