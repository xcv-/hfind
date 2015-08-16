{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module System.Posix.Find.Lang.Context
  ( VarName
  , VarId
  -- errors
  , VarNotFoundError(..)
  , TypeError(..)
  , expectedButFound
  -- monads
  , ScanT
  , EvalT
  , runScanT
  , Evaluator(..)
  -- predicates
  , NodeFunc(..)
  , FilePredicate(..)
  , DirPredicate(..)
  , NodePredicate(..)
  , EntryPredicate(..)
  -- Scan
  , ScanConfig(..)
  , readonly
  , addVar
  , getNumCaptures
  , updateNumCaptures
  , getVarId
  -- Builtins
  , Builtins
  , builtinVars
  , lookupBuiltin
  -- Eval
  , getVarValue
  , setVarValue
  , getCaptureValue
  , setCaptures
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import Control.Monad.Morph

import Data.List (findIndex)

import qualified Data.Text          as T
import qualified Data.Text.Foreign  as T

import Data.Text.ICU.Regex (Regex)
import qualified Data.Text.ICU.Regex as Regex

import qualified Data.HashMap.Strict as H

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V

import System.Posix.Text.Path (Path, Abs, File, Dir)

import System.Posix.Find.Types
import System.Posix.Find.Lang.Types

import qualified System.Posix.Find.Lang.Builtins as Builtin


type VarName = T.Text
newtype VarId = VarId Int

data ScanContext = ScanContext
    { ctxVars           :: ![VarName] -- reversed
    , ctxNumVars        :: !Int
    , ctxNumCaptures    :: !Int
    }

defScanContext :: ScanContext
defScanContext = ScanContext [] 0 0

data EvalContext = EvalContext
    { ctxValues      :: !(IOVector Value) -- reversed
    , ctxActiveRegex :: !(Maybe (Regex, T.Text))  -- not reversed
    }

defEvalContext :: Int -> IO EvalContext
defEvalContext maxVals = do
    vals <- V.new maxVals
    return (EvalContext vals Nothing)


-- TODO: error location info

data VarNotFoundError = VarNotFound       !Var
                      | CaptureOutOfRange !Int
    deriving Show

data TypeError = !T.Text `ExpectedButFound` !T.Text
    deriving Show

expectedButFound :: ValueType -> ValueType -> TypeError
t `expectedButFound` t' = typeName t `ExpectedButFound` typeName t'


-- ScanT/EvalT transformers -----------------------------------------

data ScanConfig = ScanConfig -- lazy (initialized as fixpoint)
    { scanRootDir  :: Path Abs Dir
    , scanBuiltins :: Builtins
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
    EvalT (ExceptT TypeError
            (StateT EvalContext m)
              a)
    deriving (Functor, Applicative, Monad,
              MonadError TypeError,
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


newtype Evaluator = Evaluator
    { runEvalT :: forall m a. MonadIO m => EvalT m a -> m (Either TypeError a) }

runScanT :: MonadIO m
         => Path Abs Dir
         -> ScanT m a
         -> m (Either VarNotFoundError (a, Evaluator))
runScanT root (ScanT m) = do
    let cfg = ScanConfig root (mkBuiltins cfg)

    (ma, s) <- runReaderT (runStateT (runExceptT m) defScanContext) cfg

    let run = Evaluator $ \(EvalT n) -> do
                  ctx <- liftIO $ defEvalContext (ctxNumVars s)
                  evalStateT (runExceptT n) ctx

    return $ fmap (\a -> (a, run)) ma



-- polymorphic functions/predicates ---------------------------------

newtype NodeFunc = NodeFunc
    { evalNodeFunc :: forall t. FSNode t 'Resolved -> EvalT IO Value }

newtype FilePredicate = FilePredicate
    { evalFilePredicate :: FSNode File 'Resolved -> EvalT IO Bool }

newtype DirPredicate = DirPredicate
    { evalDirPredicate :: FSNode Dir 'Resolved -> EvalT IO Bool }

newtype NodePredicate = NodePredicate
    { evalNodePredicate :: forall t. FSNode t 'Resolved -> EvalT IO Bool }

newtype EntryPredicate = EntryPredicate
    { evalEntryPredicate :: NodeListEntry 'Resolved -> EvalT IO Bool }



-- Scan primitives --------------------------------------------------

readonly :: Monad m => ScanT m a -> ScanT m a
readonly (ScanT m) = ScanT $ do
    s <- get
    a <- m
    put s
    return a


addVar :: VarName -> Scan VarId
addVar name = ScanT $ do
    modify $ \ctx ->
      ctx { ctxVars       = (name : ctxVars ctx)
          , ctxNumVars    = 1 + ctxNumVars ctx
          }
    gets (VarId . ctxNumVars)


getNumCaptures :: Scan Int
getNumCaptures = ScanT $ gets ctxNumCaptures

updateNumCaptures :: RxCaptureMode -> Regex -> Scan ()
updateNumCaptures NoCapture _  = return ()
updateNumCaptures Capture   rx = ScanT $ do
    numCaptures <- liftIO $ Regex.groupCount rx

    modify $ \ctx -> ctx { ctxNumCaptures = numCaptures }

getVarId :: VarName -> Scan (Maybe VarId)
getVarId name = ScanT $ do
    vars <- gets ctxVars

    return $ VarId <$> findIndex (==name) vars


-- Builtins ---------------------------------------------------------

newtype Builtins = Builtins (H.HashMap T.Text NodeFunc)

builtinVars :: Scan [T.Text]
builtinVars = do
    Builtins builtins <- asks scanBuiltins
    return (H.keys builtins)

lookupBuiltin :: MonadIO m => T.Text -> ScanT m (Maybe NodeFunc)
lookupBuiltin name = do
    Builtins builtins <- asks scanBuiltins
    return (H.lookup name builtins)


mkBuiltins :: ScanConfig -> Builtins
mkBuiltins root = Builtins $ H.fromList
    [ ("type",    NodeFunc (lift . Builtin.var_type))
    , ("hidden",  NodeFunc (lift . Builtin.var_hidden))
    , ("name",    NodeFunc (lift . Builtin.var_name))
    , ("path",    NodeFunc (lift . Builtin.var_path))
    , ("relpath", NodeFunc (lift . Builtin.var_relpath))
    , ("parent",  NodeFunc (lift . Builtin.var_parent))
    , ("size",    NodeFunc (lift . Builtin.var_size))
    , ("perms",   NodeFunc (lift . Builtin.var_perms))
    , ("owner",   NodeFunc (lift . Builtin.var_owner))
    , ("ownerid", NodeFunc (lift . Builtin.var_ownerid))
    , ("group",   NodeFunc (lift . Builtin.var_group))
    , ("groupid", NodeFunc (lift . Builtin.var_groupid))
    ]


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
    Just (rx, text) <- gets ctxActiveRegex

    mstart <- liftIO $ Regex.start rx i
    mend   <- liftIO $ Regex.end   rx i

    case liftA2 (,) mstart mend of
        Just (start, end) ->
            return $ T.dropWord16 (start-1) (T.takeWord16 end text)
        Nothing ->
            let pat = Regex.pattern rx in
            error $ "getCaptureValue: could not find $" ++ show i
                      ++ " of " ++ T.unpack pat


setCaptures :: RxCaptureMode -> Regex -> T.Text -> Eval ()
setCaptures NoCapture _  _ = return ()
setCaptures Capture   rx t = EvalT $ do
    modify $ \ctx -> ctx { ctxActiveRegex = Just (rx, t) }
