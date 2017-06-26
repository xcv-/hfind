{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module System.HFind.Expr.Bakers.Fused
  ( FusedBaker
  , runFusedBaker
  , runFusedBakerToString
  , bakeLetBinding
  ) where

import Data.Int (Int64)
import Data.Monoid
import Data.Type.Equality ((:~:)(..))

import Control.Applicative
import Control.Monad.Except

import Data.Text (Text)
import qualified Data.Text.Encoding as T

import qualified Data.Text.ICU as ICU

import qualified Data.Typeable as Typeable

import System.HFind.Types (FSAnyNodeR)

import System.HFind.Expr.Types
import System.HFind.Expr.Baker (Baker)
import System.HFind.Expr.Eval  (Eval)
import System.HFind.Expr.Error (BakingError(..), expectedButFound)

import System.HFind.Expr.Builtins (BuiltinVar(..), BuiltinFunc(..))

import qualified System.HFind.Expr.Baker as Baker
import qualified System.HFind.Expr.Eval  as Eval


-- A dynamic value is a function of the current node
data DynValue = forall a. IsValue a => DynValue (FSAnyNodeR -> Eval (Value a))

newtype FusedBaker = FusedBaker (Baker DynValue)


runFusedBaker :: IsValue t => FusedBaker -> Baker (FSAnyNodeR -> Eval t)
runFusedBaker (FusedBaker me) = do
    e <- typeCheckValue =<< me
    return (fmap fromValue . e)

runFusedBakerToString :: FusedBaker -> Baker (FSAnyNodeR -> Eval Text)
runFusedBakerToString (FusedBaker me) = do
    DynValue e <- me
    return (fmap valueToString . e)


instance IsLit FusedBaker where
    boolL   b _ = FusedBaker $ return $ DynValue (\_ -> return (toValue b))
    numL    n _ = FusedBaker $ return $ DynValue (\_ -> return (toValue n))
    stringL s _ = FusedBaker $ return $ DynValue (\_ -> return (toValue s))

instance IsVar FusedBaker where
    namedVar name src = FusedBaker $ Baker.frame "a variable" src $
        Baker.lookupBuiltinVar name >>= \case
            -- it's a built-in variable
            Just (BuiltinVar b) -> return (DynValue b)

            -- $f is sugar for f $_currentnode
            Nothing -> Baker.lookupBuiltinFunc name >>= \case
                Just (BuiltinFunc f) -> do
                    tc_f <- typeCheckIn f
                    return (DynValue (tc_f . toValue))

                -- a user-defined variable
                Nothing -> Baker.getVarId name >>= \case
                    Just (Baker.SomeVarId i) ->
                        return (DynValue (\_ -> Eval.getVarValue i))

                    -- fall back to environment variables
                    Nothing -> Baker.lookupEnv (T.encodeUtf8 name) >>= \case
                        Just value -> return (DynValue (\_ -> return value))
                        Nothing    -> throwError (VarNotFound (NamedVar name))

    rxCapVar i src = FusedBaker $ Baker.frame "a regex capture variable" src $ do
        ncap <- Baker.getNumCaptures
        if i < ncap
          then return (DynValue (\_ -> toValue <$> Eval.getCaptureValue i))
          else throwError (VarNotFound (RxCapVar i))


instance IsExpr FusedBaker where
    type ExprLit FusedBaker = FusedBaker
    type ExprVar FusedBaker = FusedBaker

    litE fb _ = fb
    varE fb _ = fb

    appE fname (FusedBaker me) src =
        FusedBaker $ Baker.frame "a function application" src $ do
            Baker.lookupBuiltinFunc fname >>= \case
                Just (BuiltinFunc f) -> do
                    (!e) <- typeCheckValue =<< me
                    withBacktrace $! f <=< e

                Nothing -> throwError (VarNotFound (NamedVar fname))


    plusE fb1 fb2 src =
        FusedBaker $ Baker.frame "an addition" src $ do
            (!e1) <- runFusedBaker fb1
            (!e2) <- runFusedBaker fb2

            withBacktrace $! \n -> do
                x <- e1 n
                y <- e2 n
                return (IntV (x + y))

    multE fb1 fb2 src =
        FusedBaker $ Baker.frame "a multiplication" src $ do
            (!e1) <- runFusedBaker fb1
            (!e2) <- runFusedBaker fb2

            withBacktrace $! \n -> do
                x <- e1 n
                y <- e2 n
                return (IntV (x * y))

    negE fb src =
        FusedBaker $ Baker.frame "a negation" src $ do
            (!e) <- runFusedBaker fb

            withBacktrace $! \n -> do
                x <- e n
                return (IntV (-x))

    interpE pieces src =
        FusedBaker $ Baker.frame "a string interpolation" src $ do
            values <- forM pieces $ \case
                          InterpLit text -> return (InterpLit text)
                          InterpVar fb   -> InterpVar <$> runFusedBakerToString fb

            withBacktrace $! \n -> do
                strs <- mapM (toString n) values

                return (toValue (mconcat strs))
      where
        toString _ (InterpLit s) = return s
        toString n (InterpVar e) = e n


instance IsPred FusedBaker where
    type PredExpr FusedBaker = FusedBaker

    scopeP (FusedBaker mp) src =
        FusedBaker $ Baker.frame "inner scope (scope)" src $ do
            DynValue p <- Baker.readonly mp

            return $! DynValue (Eval.readonly . p)

    notP fb src =
        FusedBaker $ Baker.frame "a negation (not)" src $ do
            -- negative predicates cannot add things to scope
            (!p) <- Baker.readonly (runFusedBaker fb)

            return $! DynValue (fmap (toValue . not) . Eval.readonly . p)

    andP fb1 fb2 src =
        FusedBaker $ Baker.frame "a conjunction (and)" src $ do
            (!p1) <- runFusedBaker fb1
            (!p2) <- runFusedBaker fb2

            return $! DynValue $ \n -> do
                r <- p1 n
                if r
                  then toValue <$> p2 n
                  else return (toValue False)

    orP fb1 fb2 src =
        FusedBaker $ Baker.frame "a disjunction (or)" src $ do
            -- we don't know which one will be accepted yet
            -- in the future we may want to intersect all possible results
            (!p1) <- Baker.readonly (runFusedBaker fb1)
            (!p2) <- Baker.readonly (runFusedBaker fb2)

            return $! DynValue $ \n -> Eval.readonly $ do
                r <- p1 n
                if r
                  then return (toValue True)
                  else toValue <$> p2 n

    exprP fb _ = fb

    opP op fb1 fb2 src =
        let opName = "(" <> opSymbol op <> ")" in

        FusedBaker $ Baker.frame ("operator " <> opName) src $ do

            let typeChecking :: (Int64 -> Int64 -> Bool) -> Baker DynValue
                typeChecking f = do
                    e1 <- runFusedBaker fb1
                    e2 <- runFusedBaker fb2
                    withBacktrace $! \n ->
                        toValue <$> liftA2 f (e1 n) (e2 n)

            case op of
                OpEQ -> do
                    let FusedBaker me1 = fb1
                        FusedBaker me2 = fb2

                    DynValue e1 <- me1
                    e2          <- typeCheckValue =<< me2

                    withBacktrace $! \n ->
                        toValue <$> liftA2 (==) (e1 n) (e2 n)

                OpLT -> typeChecking (<)
                OpLE -> typeChecking (<=)
                OpGT -> typeChecking (>)
                OpGE -> typeChecking (>=)

    matchP fb rx capMode src =
        FusedBaker $ Baker.frame "a regex match" src $ do
            e <- runFusedBaker fb
            Baker.updateNumCaptures capMode rx

            withBacktrace $! \n -> do
                s <- e n

                case ICU.find rx s of
                    Nothing    -> return (toValue False)
                    Just match -> do
                        Eval.setCaptures capMode match
                        return (toValue True)


bakeLetBinding :: Name -> FusedBaker -> Baker (FSAnyNodeR -> Eval ())
bakeLetBinding name (FusedBaker me) = do
    DynValue e <- me
    varId      <- Baker.newVar name

    return $! Eval.setVarValue varId <=< e

tryCoercion :: forall a b. (IsValue a, IsValue b) => Baker (a :~: b)
tryCoercion =
    case Typeable.eqT of
        Just refl -> return refl
        Nothing   ->
            let ta = valueTypeOf (Typeable.Proxy @a)
                tb = valueTypeOf (Typeable.Proxy @b)
            in
                throwError $ tb `expectedButFound` ta

typeCheckIn :: forall a b c. (IsValue a, IsValue b)
            => (Value a -> c) -> Baker (Value b -> c)
typeCheckIn f = do
    Refl <- tryCoercion @a @b
    return f

typeCheckValue :: forall b. IsValue b
               => DynValue -> Baker (FSAnyNodeR -> Eval (Value b))
typeCheckValue (DynValue (f :: FSAnyNodeR -> Eval (Value a))) = do
    Refl <- tryCoercion @a @b
    return f

withBacktrace :: IsValue a
              => (FSAnyNodeR -> Eval (Value a)) -> Baker DynValue
withBacktrace e = do
    bt <- Baker.getBacktrace
    return (DynValue (Eval.evalWithin bt . e))
