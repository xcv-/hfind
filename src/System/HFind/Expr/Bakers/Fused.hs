{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module System.HFind.Expr.Bakers.Fused
  ( LitBaker,  runLitBaker
  , VarBaker,  runVarBaker
  , ExprBaker, runExprBaker
  , PredBaker, runPredBaker
  ) where

import Data.Monoid

import Control.Applicative
import Control.Monad.Except

import qualified Data.Text.Encoding as T

import qualified Data.Text.ICU as ICU

import System.HFind.Types (FSAnyNodeR)

import System.HFind.Expr.Types
import System.HFind.Expr.Baker (Baker)
import System.HFind.Expr.Eval  (Eval)
import System.HFind.Expr.Error (VarNotFoundError(..),
                                RuntimeError(..), expectedButFound)

import qualified System.HFind.Expr.Baker as Baker
import qualified System.HFind.Expr.Eval  as Eval


type EvalValue = FSAnyNodeR -> Eval Value
type EvalBool  = FSAnyNodeR -> Eval Bool

newtype LitBaker  = LitBaker  { runLitBaker  :: Baker Value     }
newtype VarBaker  = VarBaker  { runVarBaker  :: Baker EvalValue }
newtype ExprBaker = ExprBaker { runExprBaker :: Baker EvalValue }
newtype PredBaker = PredBaker { runPredBaker :: Baker EvalBool  }


instance IsLit LitBaker where
    boolL   b _ = LitBaker $ return (BoolV b)
    numL    n _ = LitBaker $ return (NumV n)
    stringL s _ = LitBaker $ return (StringV s)

instance IsVar VarBaker where
    namedVar name src = VarBaker $ Baker.frame "a variable" src $
        Baker.lookupBuiltinVar name >>= \case
            -- it's a built-in variable
            Just b  -> return b

            -- $f is sugar for f $_currentnode
            Nothing -> Baker.lookupBuiltinFunc name >>= \case
                Just f  -> return (f . NodeV)

                -- a manually defined variable
                Nothing -> Baker.getVarId name >>= \case
                    Just i -> return (\_ -> Eval.getVarValue i)

                    -- fall back to environment variables
                    Nothing -> Baker.lookupEnv (T.encodeUtf8 name) >>= \case
                        Just value -> return (\_ -> return value)
                        Nothing    -> throwError (VarNotFound (NamedVar name))

    rxCapVar i src = VarBaker $ Baker.frame "a regex capture variable" src $ do
        ncap <- Baker.getNumCaptures
        if i < ncap
          then return (\_ -> StringV <$> Eval.getCaptureValue i)
          else throwError (VarNotFound (RxCapVar i))


instance IsExpr ExprBaker where
    type ExprLit ExprBaker = LitBaker
    type ExprVar ExprBaker = VarBaker

    litE (LitBaker mv) _ =
        ExprBaker $ do
            (!v) <- mv
            return (\_ -> return v)

    varE (VarBaker mv) _ =
        ExprBaker mv

    appE fname (ExprBaker me) src =
        ExprBaker $ Baker.frame "a function application" src $ do
            (!e) <- me

            bt <- Baker.getBacktrace

            Baker.lookupBuiltinFunc fname >>= \case
                Just f  -> return (Eval.evalWithin bt . f <=< e)
                Nothing -> throwError (VarNotFound (NamedVar fname))

    interpE pieces src =
        ExprBaker $ Baker.frame "a string interpolation" src $ do
            values <- forM pieces $ \case
                          InterpVar (VarBaker mv) -> InterpVar <$> mv
                          InterpLit text          -> return (InterpLit text)

            return $ \n -> do
                strs <- mapM (toString n) values

                return (StringV (mconcat strs))
      where
        toString _ (InterpLit s) = return s
        toString n (InterpVar e) = coerceToString <$> e n


instance IsPred PredBaker where
    type PredExpr PredBaker = ExprBaker

    scopeP (PredBaker mp) src =
        PredBaker $ Baker.frame "inner scope (scope)" src $ do
            (!p) <- Baker.readonly mp

            return $! Eval.readonly . p

    notP (PredBaker mp) src =
        PredBaker $ Baker.frame "a negation (not)" src $ do
            -- negative predicates cannot add things to scope
            (!p) <- Baker.readonly mp

            return $! fmap not . Eval.readonly . p

    andP (PredBaker mp1) (PredBaker mp2) src =
        PredBaker $ Baker.frame "a conjunction (&&)" src $ do
            (!p1) <- mp1
            (!p2) <- mp2

            return $ \n -> do
                r <- p1 n
                if r
                  then p2 n
                  else return False

    orP (PredBaker mp1) (PredBaker mp2) src =
        PredBaker $ Baker.frame "a disjunction (||)" src $ do
            -- we don't know which one will be accepted yet
            -- in the future we may want to intersect all possible results
            (!p1) <- Baker.readonly mp1
            (!p2) <- Baker.readonly mp2

            return $ \n -> Eval.readonly $ do
                r <- p1 n
                if r
                  then return True
                  else p2 n

    exprP (ExprBaker me) _ = PredBaker $ do
        (!e) <- me
        return $ \n -> do
            val <- e n
            case val of
                BoolV b -> return b
                _       -> throwError $
                  typeName TBool `ExpectedButFound` typeNameOf val

    opP op (ExprBaker me1) (ExprBaker me2) src =
        let opName = "(" <> opSymbol op <> ")" in

        PredBaker $ Baker.frame ("operator " <> opName) src $ do
            (!e1) <- me1
            (!e2) <- me2

            let eval n = liftA2 (,) (e1 n) (e2 n)

            let evalNumeric = eval >=> expect TNum TNum >=> \case
                    (NumV a, NumV b) -> return (a, b)
                    _ -> error "System.Posix.Find.Lang.Eval.evalNumeric"

            bt <- Baker.getBacktrace

            return $! (Eval.evalWithin bt .) $!
                case op of
                    OpEQ -> eval >=> \case
                        (BoolV   a, BoolV   b) -> return (a == b)
                        (NumV    a, NumV    b) -> return (a == b)
                        (StringV a, StringV b) -> return (a == b)
                        (NodeV   a, NodeV   b) -> return (a == b)
                        (a, b) -> throwError $
                            "comparable pair" `ExpectedButFound`
                              (typeNameOf a <> " and " <> typeNameOf b)

                    OpLT -> evalNumeric >=> \(a, b) -> return (a < b)
                    OpLE -> evalNumeric >=> \(a, b) -> return (a <= b)
                    OpGT -> evalNumeric >=> \(a, b) -> return (a > b)
                    OpGE -> evalNumeric >=> \(a, b) -> return (a >= b)
      where
        expect :: ValueType -> ValueType -> (Value, Value) -> Eval (Value, Value)
        expect t1 t2 (a, b)
          | t1 /= typeOf a = throwError $ t1 `expectedButFound` typeOf a
          | t2 /= typeOf b = throwError $ t2 `expectedButFound` typeOf b
          | otherwise      = return (a,b)

    matchP (ExprBaker me) rx capMode src =
        PredBaker $ Baker.frame "a regex match" src $ do
            (!e) <- me
            Baker.updateNumCaptures capMode rx

            return $ \n -> do
                s <- coerceToString <$> e n

                case ICU.find rx s of
                    Nothing    -> return False
                    Just match -> do
                        Eval.setCaptures capMode match
                        return True
