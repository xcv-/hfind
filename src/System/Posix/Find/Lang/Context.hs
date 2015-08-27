{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module System.Posix.Find.Lang.Context
    ( VarId
    -- monads
    , Baker
    , BakerT
    , runBakerT
    , Eval
    , Evaluator(..)
    -- Baker
    , BakerConfig(..)
    , readonly
    , addVar
    , getNumCaptures
    , updateNumCaptures
    , getVarId
    , lookupEnv
    -- Builtins
    , lookupBuiltinVar
    , lookupBuiltinFunc
    -- Eval
    , getVarValue
    , setVarValue
    , getCaptureValue
    , setCaptures
    ) where

import Data.Functor.Identity (Identity)

import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import Control.Monad.Morph

import Data.List (elemIndex)

import Data.ByteString (ByteString)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import qualified Data.Text.ICU as ICU

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V

import qualified System.Posix.Env.ByteString as Posix

import System.Posix.Text.Path (Path, Abs, Dir)

import System.Posix.Find.Lang.Types (Name, Value(..), RxCaptureMode(..),
                                     VarNotFoundError, RuntimeError)

import qualified System.Posix.Find.Lang.Builtins as Builtins


newtype VarId = VarId Int

data BakingContext = BakingContext
    { ctxVars           :: ![Name] -- reversed
    , ctxNumVars        :: !Int
    , ctxNumCaptures    :: !Int
    }

defBakingContext :: BakingContext
defBakingContext = BakingContext [] 0 0

data EvalContext = EvalContext
    { ctxValues      :: !(IOVector Value) -- reversed
    , ctxActiveMatch :: !(Maybe ICU.Match)
    }

defEvalContext :: Int -> IO EvalContext
defEvalContext maxVals = do
    vals <- V.new maxVals
    return (EvalContext vals Nothing)


-- BakerT/EvalT transformers -----------------------------------------

data BakerConfig = BakerConfig
    { bakerRoot     :: Path Abs Dir
    , bakerBuiltins :: Builtins.Builtins (EvalT IO)
    , bakerSysEnv   :: [(ByteString, Value)]
    }

type Baker a = BakerT Identity a

newtype BakerT m a =
    BakerT (ExceptT VarNotFoundError
             (StateT BakingContext
               (ReaderT BakerConfig m))
                 a)
    deriving (Functor, Applicative, Monad,
              MonadError VarNotFoundError,
              MonadReader BakerConfig,
              MonadIO)

type Eval = EvalT IO

newtype EvalT m a =
    EvalT (ExceptT RuntimeError
            (StateT EvalContext m)
              a)
    deriving (Functor, Applicative, Monad,
              MonadError RuntimeError,
              MonadIO, MonadThrow, MonadCatch)

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


newtype Evaluator = Evaluator
    { runEvalT :: forall m a. MonadIO m => EvalT m a -> m (Either RuntimeError a) }

runBakerT :: MonadIO m
          => Path Abs Dir
          -> BakerT m a
          -> m (Either VarNotFoundError (a, Evaluator))
runBakerT root (BakerT m) = do
    env <- liftIO Posix.getEnvironment

    let cfg = BakerConfig {
          bakerRoot     = root
        , bakerBuiltins = Builtins.mkBuiltins root
        , bakerSysEnv   = map (\(k,v) -> (k, StringV (T.decodeUtf8 v))) env
        }

    (ma, s) <- runReaderT (runStateT (runExceptT m) defBakingContext) cfg

    let run = Evaluator $ \(EvalT n) -> do
                  ctx <- liftIO $ defEvalContext (ctxNumVars s)
                  evalStateT (runExceptT n) ctx

    return $ fmap (\a -> (a, run)) ma


-- Baker primitives --------------------------------------------------

readonly :: Monad m => BakerT m a -> BakerT m a
readonly (BakerT m) = BakerT $ do
    s <- get
    a <- m
    put s
    return a


addVar :: Monad m => Name -> BakerT m VarId
addVar name = BakerT $ do
    modify $ \ctx ->
      ctx { ctxVars       = name : ctxVars ctx
          , ctxNumVars    = 1 + ctxNumVars ctx
          }
    gets (VarId . ctxNumVars)


getNumCaptures :: Monad m => BakerT m Int
getNumCaptures = BakerT $ gets ctxNumCaptures

updateNumCaptures :: Monad m => RxCaptureMode -> ICU.Regex -> BakerT m ()
updateNumCaptures NoCapture _  = return ()
updateNumCaptures Capture   rx = BakerT $ do
    let numCaptures = ICU.groupCount rx

    -- +1 because the full pattern is $0
    modify $ \ctx -> ctx { ctxNumCaptures = numCaptures+1 }

getVarId :: Monad m => Name -> BakerT m (Maybe VarId)
getVarId name = BakerT $ do
    vars <- gets ctxVars

    return $ VarId <$> elemIndex name vars



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



-- Eval primitives --------------------------------------------------

getVarValue :: MonadIO m => VarId -> EvalT m Value
getVarValue (VarId i) = EvalT $ do
    vals <- gets ctxValues
    liftIO $ V.read vals i

setVarValue :: MonadIO m => VarId -> Value -> EvalT m ()
setVarValue (VarId i) val = EvalT $ do
    vars <- gets ctxValues
    liftIO $ V.write vars i val
    return ()


getCaptureValue :: Monad m => Int -> EvalT m T.Text
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
