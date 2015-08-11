{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module System.Posix.Find.Lang.Eval
  ( LitScan,  runLitScan
  , VarScan,  runVarScan
  , ExprScan, runExprScan
  , PredScan, runPredScan
  ) where

import Data.Int
import Data.Functor
import Data.Monoid

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Data.Text.ICU.Regex (Regex)
import qualified Data.Text.ICU.Regex as Regex

import qualified Data.Vector.Mutable as V

import qualified System.Posix.ByteString as Posix

import System.Posix.Find.Types

import System.Posix.Find.Lang.Types
import System.Posix.Find.Lang.Builtins


newtype LitScan  m   = LitScan  { runLitScan  :: m Lit               }
newtype VarScan  m n = VarScan  { runVarScan  :: m (NodeFunc n)      }
newtype ExprScan m n = ExprScan { runExprScan :: m (NodeFunc n)      }
newtype PredScan m n = PredScan { runPredScan :: m (NodePredicate n) }


readonly :: MonadState s m => m a -> m a
readonly m = do
    s <- get
    a <- m
    put s
    return a


addVar :: Scanning m => VarName -> m VarId
addVar name = do
    modify $ ctx ->
      let numVars = 1 + ctxNumVars in
      in ctx { ctxVars       = (name : ctxVars ctx)
             , ctxNumVars    = numVars
             , ctxMaxNumVars = max (ctxMaxNumVars ctx) numVars


getNumCaptures :: Scanning m => m Int
getNumCaptures = gets ctxNumCaptures

updateNumCaptures :: Scanning m => RxCaptureMode -> Regex -> m ()
updateNumCaptures NoCapture _  = return ()
updateNumCaptures Capture   rx = do
    numCaptures <- Regex.groupCount rx

    modify $ \ctx ->
      ctx { ctxNumCaptures    = numCaptures
          , ctxMaxNumCaptures = max numCaptures (ctxMaxNumCaptures ctx)
          }

getVarId :: Scanning m => VarName -> m (Maybe VarId)
getVarId name = do
    vars <- gets ctxVars

    return (findIndex (==name) vars)


getVarValue :: Evaluating n => VarId -> n Lit
getVarValue i = do
    ctx <- get
    let n = ctxNumValues ctx
        x = ctxValues ctx !! (n-i) -- TODO error handling
    return x

setVarValue :: Evaluating n => VarId -> Lit -> n ()
setVarValue i val = do
    vars <- gets ctxValues
    V.write vars i val
    return ()


getCaptureValue :: Evaluating n => Int -> n T.Text
getCaptureValue i = do
    cap <- gets ctxCaptures
    val <- V.read cap i -- check already done
    return val

setCaptures :: Evaluating n => RxCaptureMode -> Regex -> n ()
setCaptures NoCapture _  = return ()
setCaptures Capture   rx = do
    modify $ \ctx -> ctx { ctxActiveRegex = rx }

    -- NOTE: ignore non-capturing regexes


instance Scanning m => IsLit (LitScan m) where
    boolL      = LitScan . return . BoolL
    numL       = LitScan . return . NumL
    stringL    = LitScan . return . StringL
    regexL r c = LitScan $ case c of
                   Capture   -> updateNumCaptures r $> RegexL r c
                   NoCapture -> return (RegexL r c)

instance ScanningFor m n => IsVar (VarScan m n) where
    namedVar name = VarScan $
        case lookupBuiltin name of
          Just b -> return b

          _ -> getVarId name >>= \case
                 Just i  ->
                     return $ NodeFunc (\_ -> getVarValue i)

                 -- lookup in environment variables
                 Nothing -> do
                     let bname = T.encodeUtf8 name

                     liftIO (Posix.getEnv bname) >>= \case
                         Just bvalue -> do
                             let value = T.decodeUtf8 bvalue
                             return $ NodeFunc (\_ -> return (StringL value))
                         Nothing ->
                             throwError (VarNotFound (namedVar name))

    rxCapVar i = VarScan $ do
        ncap <- getNumCaptures
        if i < ncap
          then return $ NodeFunc (\_ -> StringL <$> getCaptureValue i)
          else throwError (VarNotFound (rxCapVar i))


instance ScanningFor m n => IsExpr (ExprScan m n) where
    type ExprLit (ExprScan m n) = LitScan m
    type ExprVar (ExprScan m n) = VarScan m n

    litE (LitScan ml) = ExprScan $ do
        (!l) <- ml
        return $ NodeFunc (\_ -> return l)

    varE (VarScan mv) = ExprScan mv

    interpE pieces = ExprScan $ do
        checkedPieces <-
            forM pieces $ \case
                Right (VarScan mv) -> Right <$> mv
                Left text               -> return (Left text)

        return $ NodeFunc $ \n -> do
            strs <- forM checkedPieces $ \case
                      Left text -> return text
                      Right fn  -> toString =<< evalNodeFunc fn n

            return (StringL (mconcat strs))
      where
        toString = \case
          BoolL   True  -> return "true"
          BoolL   False -> return "false"
          NumL    x     -> return (T.pack (show x))
          StringL s     -> return s
          RegexL  _ _   -> throwError $
              "convertible to text" `ExpectedButFound` typeName TRegex


instance ScanningFor m n => IsPred (PredScan m n) where
    type PredExpr (PredScan m n) = ExprScan m n

    notP (PredScan mp) = PredScan $ do
        -- negative predicates cannot add things to scope
        (!p) <- readonly mp
        return $! NodePredicate (fmap not . evalNodePredicate p)

    andP (PredScan mp1) (PredScan mp2) = PredScan $ do
        (!p1) <- mp1
        (!p2) <- mp2
        return $! NodePredicate $ \n -> do
            r <- evalNodePredicate p1 n
            if r
              then evalNodePredicate p2 n
              else return False

    orP (PredScan mp1) (PredScan mp2) = PredScan $ do
        -- we don't know which one will be accepted yet
        -- in the future we may want to intersect all possible results
        (!p1) <- readonly mp1
        (!p2) <- readonly mp2
        return $! NodePredicate $ \n -> do
            r <- evalNodePredicate p1 n
            if r
              then return True
              else evalNodePredicate p2 n

    exprP (ExprScan me) = PredScan $ do
        (!e) <- me
        return $! NodePredicate $ \n -> do
            l <- evalNodeFunc e n
            case l of
                BoolL b -> return b
                _       -> throwError $
                  typeName TBool `ExpectedButFound` typeNameOf l

    opP op (ExprScan me1) (ExprScan me2) = PredScan $ do
        (!e1) <- me1
        (!e2) <- me2

        let eval :: FSNode t 'Resolved -> n (Lit, Lit)
            eval n = liftA2 (,) (evalNodeFunc e1 n) (evalNodeFunc e2 n)

        let evalNumeric :: FSNode t 'Resolved -> n (Int64, Int64)
            evalNumeric = eval >=> expect TNum TNum >=> \case
                (NumL a, NumL b) -> return (a, b)
                _ -> error "System.Posix.Find.Lang.Eval.evalNumeric"

        let evalStringRx :: FSNode t 'Resolved -> n (T.Text, Regex)
            evalStringRx = eval >=> expect TString TRegex >=> \case
                (StringL s, RegexL rx _) -> return (s, rx)
                _ -> error "System.Posix.Find.Lang.Eval.evalStringRx"

        return $! NodePredicate $
          case op of
            OpEQ -> eval >=> \case
              (BoolL   a, BoolL   b) -> return (a == b)
              (NumL    a, NumL    b) -> return (a == b)
              (StringL a, StringL b) -> return (a == b)

              (a, b) -> throwError $ "comparable pair"
                  `ExpectedButFound` (typeNameOf a <> " and " <> typeNameOf b)

            OpLT -> evalNumeric >=> \(a, b) -> return (a < b)
            OpLE -> evalNumeric >=> \(a, b) -> return (a <= b)
            OpGT -> evalNumeric >=> \(a, b) -> return (a > b)
            OpGE -> evalNumeric >=> \(a, b) -> return (a >= b)

            OpRX -> evalStringRx >=> \(s, rx) -> do
                liftIO $ Regex.setText rx s
                matches <- liftIO $ Regex.find rx 0
                when matches (setCaptures rx)
                return matches
      where
        expect :: ExprType -> ExprType -> (Lit, Lit) -> n (Lit, Lit)
        expect t1 t2 (a,b)
          | t1 /= typeOf a = throwError $ t1 `expectedButFound` typeOf a
          | t2 /= typeOf b = throwError $ t2 `expectedButFound` typeOf b
          | otherwise      = return (a,b)


{-
processLit :: Scanning m => Lit -> m Lit
processLit lit@(RegexL rx) = updateNumCaptures rx $> lit
processLit lit             = return lit

processExpr :: ScanningFor m n => Expr -> m (NodeFunc n)
processExpr (LitE l) = processLit l $> NodeFunc (\_ -> return l)
processExpr (InterpE s) = do
    let injE = either (LitE . StringL) VarE

    preprocessed <- mapM (processExpr . injE) s

    return $ NodeFunc $ \n ->
        fmap (StringL . mconcat) $
          mapM (asText <=< evalExprOn n) preprocessed
  where
    asText :: Evaluating n => Lit -> n T.Text
    asText (BoolL True)  = return "true"
    asText (BoolL False) = return "false"
    asText (NumL i)      = return (T.pack (show i))
    asText (StringL t)   = return t
    asText (RegexL _)    = throwError $
        "convertible to text" `ExpectedButFound` typeName TRegex

processExpr (VarE v@(RxCapture i)) = do
    ncapt <- gets ctxNumCaptures

    when (i >= ncapt) $ throwError (VarNotFound v)

    return $ NodeFunc (\_ -> StringL <$> getCaptureValue i)

processExpr (VarE v@(NamedVar name)) =
    case lookupBuiltin name of
      Just b -> return b

      _ -> getVarId name >>= \case
             Just i  -> return $ NodeFunc (\_ -> getVarValue i)
             Nothing -> do
                 let bname = T.encodeUtf8 name
                 liftIO (Posix.getEnv bname) >>= \case
                     Just bvalue -> do
                         let value = T.decodeUtf8 bvalue
                         return $ NodeFunc (\_ -> return (StringL value))
                     Nothing ->
                         throwError (VarNotFound v)


processPred :: (ScanningFor m n, MonadIO n) => Pred -> m (NodePredicate n)
processPred (Not p) = do
    (!p') <- readonly $ processPred p

    return $ NodePredicate $ \n ->
        fmap not (evalNodePredicate p' n)

processPred (And p q) = do
    (!p') <- processPred p
    (!q') <- processPred q

    return $ NodePredicate $ \n -> do
        r <- evalNodePredicate p' n
        if r
          then evalNodePredicate q' n
          else return False

processPred (Or p q) = do
    (!p') <- readonly $ processPred p
    (!q') <- readonly $ processPred q

    return $ NodePredicate $ \n -> do
        r <- evalNodePredicate p' n
        if r
          then return True
          else evalNodePredicate q' n

processPred (OpP e1 op e2) = do
    (!e1') <- processExpr e1
    (!e2') <- processExpr e2

    let eval n = liftM2 (,) (evalExprOn n e1') (evalExprOn n e2')

    let expect t1 t2 (a,b)
         | t1 /= typeOf a = throwError $ t1 `expectedButFound` typeOf a
         | t2 /= typeOf b = throwError $ t2 `expectedButFound` typeOf b
         | otherwise      = return (a,b)

    return $ NodePredicate $ \n ->
      case op of
        OpEQ -> eval n >>= \case
          (BoolL   a, BoolL   b) -> return (a == b)
          (NumL    a, NumL    b) -> return (a == b)
          (StringL a, StringL b) -> return (a == b)

          (a, b) -> throwTypeError $ "comparable pair"
              `ExpectedButFound` (typeNameOf a <> " and " <> typeNameOf b)

        OpLT -> eval n >>= expect TNum TNum <&> \(NumL a, NumL b) -> a <  b
        OpLE -> eval n >>= expect TNum TNum <&> \(NumL a, NumL b) -> a <= b
        OpGT -> eval n >>= expect TNum TNum <&> \(NumL a, NumL b) -> a >  b
        OpGE -> eval n >>= expect TNum TNum <&> \(NumL a, NumL b) -> a >= b

        OpRX -> eval n >>= expect TString TRegex >>= \(StringL s, RegexL rx) -> do
          liftIO $ Regex.setText rx s
          matches <- liftIO $ Regex.find rx 0
          when matches (setActiveRegex rx)
          return matches
  -}
