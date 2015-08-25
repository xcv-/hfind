{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module System.Posix.Find.Lang.Context
    ( VarId
    -- monads
    , ScanT
    , EvalT
    , runScanT
    , Evaluator(..)
    -- Scan
    , ScanConfig(..)
    , readonly
    , addVar
    , getNumCaptures
    , updateNumCaptures
    , getVarId
    -- Builtins
    , lookupBuiltinVar
    , lookupBuiltinFunc
    -- Eval
    , getVarValue
    , setVarValue
    , getCaptureValue
    , setCaptures
    ) where

import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import Control.Monad.Morph

import Data.List (elemIndex)

import qualified Data.Text          as T

import qualified Data.Text.ICU as ICU

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V

import System.Posix.Text.Path (Path, Abs, Dir)

import System.Posix.Find.Lang.Types

import qualified System.Posix.Find.Lang.Builtins as Builtins


newtype VarId = VarId Int

data ScanContext = ScanContext
    { ctxVars           :: ![Name] -- reversed
    , ctxNumVars        :: !Int
    , ctxNumCaptures    :: !Int
    }

defScanContext :: ScanContext
defScanContext = ScanContext [] 0 0

data EvalContext = EvalContext
    { ctxValues      :: !(IOVector Value) -- reversed
    , ctxActiveMatch :: !(Maybe ICU.Match)
    }

defEvalContext :: Int -> IO EvalContext
defEvalContext maxVals = do
    vals <- V.new maxVals
    return (EvalContext vals Nothing)


-- ScanT/EvalT transformers -----------------------------------------

data ScanConfig = ScanConfig
    { scanRoot     :: Path Abs Dir
    , scanBuiltins :: Builtins.Builtins (EvalT IO)
    }

newtype ScanT m a =
    ScanT (ExceptT VarNotFoundError
            (StateT ScanContext
              (ReaderT ScanConfig m))
                a)
    deriving (Functor, Applicative, Monad,
              MonadError VarNotFoundError,
              MonadReader ScanConfig,
              MonadIO)

newtype EvalT m a =
    EvalT (ExceptT RuntimeError
            (StateT EvalContext m)
              a)
    deriving (Functor, Applicative, Monad,
              MonadError RuntimeError,
              MonadIO, MonadThrow, MonadCatch)

instance MonadTrans ScanT where
    lift = ScanT . lift . lift . lift

instance MFunctor ScanT where
    hoist f (ScanT m) = ScanT $ hoist (hoist (hoist f)) m

instance MonadTrans EvalT where
    lift = EvalT . lift . lift

instance MFunctor EvalT where
    hoist f (EvalT m) = EvalT $ hoist (hoist f) m

type Eval a = forall m. MonadIO m => EvalT m a
type Scan a = forall m. MonadIO m => ScanT m a

type BuiltinVar  = Builtins.BuiltinVar  (EvalT IO)
type BuiltinFunc = Builtins.BuiltinFunc (EvalT IO)


newtype Evaluator = Evaluator
    { runEvalT :: forall m a. MonadIO m => EvalT m a -> m (Either RuntimeError a) }

runScanT :: MonadIO m
         => Path Abs Dir
         -> ScanT m a
         -> m (Either VarNotFoundError (a, Evaluator))
runScanT root (ScanT m) = do
    let cfg = ScanConfig root (Builtins.mkBuiltins root)

    (ma, s) <- runReaderT (runStateT (runExceptT m) defScanContext) cfg

    let run = Evaluator $ \(EvalT n) -> do
                  ctx <- liftIO $ defEvalContext (ctxNumVars s)
                  evalStateT (runExceptT n) ctx

    return $ fmap (\a -> (a, run)) ma


-- Scan primitives --------------------------------------------------

readonly :: Monad m => ScanT m a -> ScanT m a
readonly (ScanT m) = ScanT $ do
    s <- get
    a <- m
    put s
    return a


addVar :: Name -> Scan VarId
addVar name = ScanT $ do
    modify $ \ctx ->
      ctx { ctxVars       = name : ctxVars ctx
          , ctxNumVars    = 1 + ctxNumVars ctx
          }
    gets (VarId . ctxNumVars)


getNumCaptures :: Scan Int
getNumCaptures = ScanT $ gets ctxNumCaptures

updateNumCaptures :: RxCaptureMode -> ICU.Regex -> Scan ()
updateNumCaptures NoCapture _  = return ()
updateNumCaptures Capture   rx = ScanT $ do
    let numCaptures = ICU.groupCount rx

    -- +1 because the full pattern is $0
    modify $ \ctx -> ctx { ctxNumCaptures = numCaptures+1 }

getVarId :: Name -> Scan (Maybe VarId)
getVarId name = ScanT $ do
    vars <- gets ctxVars

    return $ VarId <$> elemIndex name vars


lookupBuiltinVar :: Name -> Scan (Maybe BuiltinVar)
lookupBuiltinVar name = ScanT $ do
    builtins <- asks scanBuiltins
    return $! Builtins.lookupVar builtins name

lookupBuiltinFunc :: Name -> Scan (Maybe BuiltinFunc)
lookupBuiltinFunc name = ScanT $ do
    builtins <- asks scanBuiltins
    return $! Builtins.lookupFunc builtins name



-- Eval primitives --------------------------------------------------

getVarValue :: VarId -> Eval Value
getVarValue (VarId i) = EvalT $ do
    vals <- gets ctxValues
    liftIO $ V.read vals i

setVarValue :: VarId -> Value -> Eval ()
setVarValue (VarId i) val = EvalT $ do
    vars <- gets ctxValues
    liftIO $ V.write vars i val
    return ()


getCaptureValue :: Int -> Eval T.Text
getCaptureValue i = EvalT $ do
    Just match <- gets ctxActiveMatch

    case ICU.group i match of
        Just text -> return text
        Nothing ->
            error $ "getCaptureValue: could not find $" ++ show i
                      ++ " of " ++ T.unpack (ICU.pattern match)


setCaptures :: RxCaptureMode -> ICU.Match -> Eval ()
setCaptures NoCapture _     = return ()
setCaptures Capture   match = EvalT $
    modify $ \ctx -> ctx { ctxActiveMatch = Just match }
