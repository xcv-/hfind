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
import Data.Monoid

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Except

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Data.Text.ICU.Regex (Regex)
import qualified Data.Text.ICU.Regex as Regex

import qualified System.Posix.ByteString as Posix

import qualified System.Posix.Text.Path as Path
import System.Posix.Find.Types

import System.Posix.Find.Lang.Types
import System.Posix.Find.Lang.Context


newtype LitScan  = LitScan  { runLitScan  :: ScanT IO Lit           }
newtype VarScan  = VarScan  { runVarScan  :: ScanT IO NodeFunc      }
newtype ExprScan = ExprScan { runExprScan :: ScanT IO NodeFunc      }
newtype PredScan = PredScan { runPredScan :: ScanT IO NodePredicate }


coerceToString :: Value -> T.Text
coerceToString = \case
    BoolV   True  -> "true"
    BoolV   False -> "false"
    NumV    x     -> T.pack (show x)
    StringV s     -> s
    NodeV   n     -> Path.toText (nodePath n)


litValue :: Lit -> Value
litValue (BoolL   b) = BoolV   b
litValue (NumL    n) = NumV    n
litValue (StringL s) = StringV s


instance IsLit LitScan where
    boolL      = LitScan . return . BoolL
    numL       = LitScan . return . NumL
    stringL    = LitScan . return . StringL

instance IsVar VarScan where
    namedVar name = VarScan $
        lookupBuiltin name >>= \case
            Just b  -> return b

            Nothing -> getVarId name >>= \case
                Just i  -> return $ NodeFunc (\_ -> getVarValue i)

                -- lookup in environment variables
                Nothing -> do
                    let bname = T.encodeUtf8 name

                    liftIO (Posix.getEnv bname) >>= \case
                        Just bvalue -> do
                            let !value = T.decodeUtf8 bvalue
                            return $! NodeFunc $ \_ ->
                                return (StringV value)
                        Nothing ->
                            throwError (VarNotFound (namedVar name))

    rxCapVar i = VarScan $ do
        ncap <- getNumCaptures
        if i < ncap
          then return $ NodeFunc (\_ -> StringV <$> getCaptureValue i)
          else throwError (VarNotFound (rxCapVar i))


instance IsExpr ExprScan where
    type ExprLit ExprScan = LitScan
    type ExprVar ExprScan = VarScan

    litE (LitScan ml) = ExprScan $ do
        (!val) <- litValue <$> ml
        return $ NodeFunc (\_ -> return val)

    varE (VarScan mv) = ExprScan mv

    interpE pieces = ExprScan $ do
        checkedPieces <-
            forM pieces $ \case
                Right (VarScan mv) -> Right <$> mv
                Left text               -> return (Left text)

        return $ NodeFunc $ \n -> do
            strs <- mapM (toString n) checkedPieces

            return (StringV (mconcat strs))
      where
        toString _ (Left s)  = return s
        toString n (Right e) = coerceToString <$> evalNodeFunc e n


instance IsPred PredScan where
    type PredExpr PredScan = ExprScan

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
            val <- evalNodeFunc e n
            case val of
                BoolV b -> return b
                _       -> throwError $
                  typeName TBool `ExpectedButFound` typeNameOf val

    opP op (ExprScan me1) (ExprScan me2) = PredScan $ do
        (!e1) <- me1
        (!e2) <- me2

        let eval :: FSNode t 'Resolved -> EvalT IO (Value, Value)
            eval n = liftA2 (,) (evalNodeFunc e1 n) (evalNodeFunc e2 n)

        let evalNumeric :: FSNode t 'Resolved -> EvalT IO (Int64, Int64)
            evalNumeric = eval >=> expect TNum TNum >=> \case
                (NumV a, NumV b) -> return (a, b)
                _ -> error "System.Posix.Find.Lang.Eval.evalNumeric"

        return $! NodePredicate $
            case op of
                OpEQ -> eval >=> \case
                    (BoolV   a, BoolV   b) -> return (a == b)
                    (NumV    a, NumV    b) -> return (a == b)
                    (StringV a, StringV b) -> return (a == b)
                    (NodeV   a, NodeV   b) -> return $ case (a, b) of
                        (FileNode stat p, FileNode stat' p') ->
                            FileNode stat p == FileNode stat' p'
                        (DirNode stat p, DirNode stat' p') ->
                            DirNode stat p == DirNode stat' p'
                        (_, _) ->
                            False


                    (a, b) -> throwError $
                        "comparable pair" `ExpectedButFound`
                          (typeNameOf a <> " and " <> typeNameOf b)

                OpLT -> evalNumeric >=> \(a, b) -> return (a < b)
                OpLE -> evalNumeric >=> \(a, b) -> return (a <= b)
                OpGT -> evalNumeric >=> \(a, b) -> return (a > b)
                OpGE -> evalNumeric >=> \(a, b) -> return (a >= b)
      where
        expect :: ValueType -> ValueType
               -> (Value, Value) -> EvalT IO (Value, Value)
        expect t1 t2 (a,b)
          | t1 /= typeOf a = throwError $ t1 `expectedButFound` typeOf a
          | t2 /= typeOf b = throwError $ t2 `expectedButFound` typeOf b
          | otherwise      = return (a,b)

    matchP (ExprScan me) rx capMode = PredScan $ do
        (!e) <- me

        return $! NodePredicate $ \n -> do
            val <- evalNodeFunc e n

            case val of
                StringV s -> do
                    liftIO $ Regex.setText rx s
                    matches <- liftIO $ Regex.find rx 0
                    when matches (setCaptures capMode rx s)
                    return matches

                v -> throwError $ TString `expectedButFound` typeOf v
