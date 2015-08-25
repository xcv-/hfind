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

import qualified Data.Text.ICU as ICU

import qualified System.Posix.ByteString as Posix

import qualified System.Posix.Text.Path as Path

import System.Posix.Find.Types

import System.Posix.Find.Lang.Types
import System.Posix.Find.Lang.Context


type EvalValue = FSAnyNode 'Resolved -> EvalT IO Value
type EvalBool  = FSAnyNode 'Resolved -> EvalT IO Bool

newtype LitScan  = LitScan  { runLitScan  :: ScanT IO Lit       }
newtype VarScan  = VarScan  { runVarScan  :: ScanT IO EvalValue }
newtype ExprScan = ExprScan { runExprScan :: ScanT IO EvalValue }
newtype PredScan = PredScan { runPredScan :: ScanT IO EvalBool  }


coerceToString :: Value -> T.Text
coerceToString = \case
    BoolV   True        -> "true"
    BoolV   False       -> "false"
    NumV    x           -> T.pack (show x)
    StringV s           -> s
    NodeV   (AnyNode n) -> Path.toText (nodePath n)


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
        lookupBuiltinVar name >>= \case
            -- it's built-in variable
            Just b  -> return b

            Nothing -> lookupBuiltinFunc name >>= \case
                -- $f is sugar for f <<current node>>
                Just f  -> return (f . NodeV)

                Nothing -> getVarId name >>= \case
                    -- a manually defined variable
                    Just i -> return (\_ -> getVarValue i)

                    -- fall back to environment variables
                    Nothing -> do
                        let bname = T.encodeUtf8 name

                        liftIO (Posix.getEnv bname) >>= \case
                            Just bvalue -> do
                                let !value = T.decodeUtf8 bvalue
                                return (\_ -> return (StringV value))
                            Nothing ->
                                throwError (VarNotFound (namedVar name))

    rxCapVar i = VarScan $ do
        ncap <- getNumCaptures
        if i < ncap
          then return (\_ -> StringV <$> getCaptureValue i)
          else throwError (VarNotFound (rxCapVar i))


instance IsExpr ExprScan where
    type ExprLit ExprScan = LitScan
    type ExprVar ExprScan = VarScan

    litE (LitScan ml) = ExprScan $ do
        (!val) <- litValue <$> ml
        return (\_ -> return val)

    varE (VarScan mv) = ExprScan mv

    appE fname (ExprScan me) = ExprScan $ do
        (!e) <- me

        lookupBuiltinFunc fname >>= \case
            Just f  -> return (f <=< e)
            Nothing -> throwError (VarNotFound (namedVar fname))


    interpE pieces = ExprScan $ do
        values <- forM pieces $ \case
                      Right (VarScan mv) -> Right <$> mv
                      Left text          -> return (Left text)

        return $ \n -> do
            strs <- mapM (toString n) values

            return (StringV (mconcat strs))
      where
        toString _ (Left s)  = return s
        toString n (Right e) = coerceToString <$> e n


instance IsPred PredScan where
    type PredExpr PredScan = ExprScan

    notP (PredScan mp) = PredScan $ do
        -- negative predicates cannot add things to scope
        (!p) <- readonly mp
        return $! (fmap not . p)

    andP (PredScan mp1) (PredScan mp2) = PredScan $ do
        (!p1) <- mp1
        (!p2) <- mp2
        return $ \n -> do
            r <- p1 n
            if r
              then p2 n
              else return False

    orP (PredScan mp1) (PredScan mp2) = PredScan $ do
        -- we don't know which one will be accepted yet
        -- in the future we may want to intersect all possible results
        (!p1) <- readonly mp1
        (!p2) <- readonly mp2
        return $ \n -> do
            r <- p1 n
            if r
              then return True
              else p2 n

    exprP (ExprScan me) = PredScan $ do
        (!e) <- me
        return $ \n -> do
            val <- e n
            case val of
                BoolV b -> return b
                _       -> throwError $
                  typeName TBool `ExpectedButFound` typeNameOf val

    opP op (ExprScan me1) (ExprScan me2) = PredScan $ do
        (!e1) <- me1
        (!e2) <- me2

        let eval :: FSAnyNode 'Resolved -> EvalT IO (Value, Value)
            eval n = liftA2 (,) (e1 n) (e2 n)

        let evalNumeric :: FSAnyNode 'Resolved -> EvalT IO (Int64, Int64)
            evalNumeric = eval >=> expect TNum TNum >=> \case
                (NumV a, NumV b) -> return (a, b)
                _ -> error "System.Posix.Find.Lang.Eval.evalNumeric"

        return $!
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
        expect :: ValueType -> ValueType
               -> (Value, Value) -> EvalT IO (Value, Value)
        expect t1 t2 (a, b)
          | t1 /= typeOf a = throwError $ t1 `expectedButFound` typeOf a
          | t2 /= typeOf b = throwError $ t2 `expectedButFound` typeOf b
          | otherwise      = return (a,b)

    matchP (ExprScan me) rx capMode = PredScan $ do
        (!e) <- me
        updateNumCaptures capMode rx

        return $ \n -> do
            s <- coerceToString <$> e n

            case ICU.find rx s of
                Nothing    -> return False
                Just match -> do
                    setCaptures capMode match
                    return True
