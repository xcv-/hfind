{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module System.HFind.Expr.Bakers.FromAST where

import System.HFind.Expr.Types.AST


mapExpr :: (Functor f, Functor g) => (forall a. f a -> g a) -> Expr_ f -> Expr_ g
mapExpr mu e =
    case e of
        LitE flit   -> LitE (mu flit)
        VarE fvar   -> VarE (mu fvar)
        AppE h fx   -> AppE h (go fx)
        PlusE x y   -> PlusE (go x) (go y)
        MultE x y   -> MultE (go x) (go y)
        NegE x      -> NegE (go x)
        InterpE fvs -> InterpE (map (fmap mu) fvs)
  where
    go = mu . fmap (mapExpr mu)

mapPred :: (Functor f, Functor g) => (forall a. f a -> g a) -> Pred_ f -> Pred_ g
mapPred mu p =
    case p of
        ScopeP fp      -> ScopeP (go fp)
        NotP fp        -> NotP (go fp)
        AndP fp fq     -> AndP (go fp) (go fq)
        OrP fp fq      -> OrP (go fp) (go fq)
        ExprP fe       -> ExprP (goExpr fe)
        MatchP fe rx m -> MatchP (goExpr fe) rx m
        OpP op fp fq   -> OpP op (goExpr fp) (goExpr fq)
  where
    go = mu . fmap (mapPred mu)
    goExpr = mu . fmap (mapExpr mu)

convertLit :: IsLit lit => LocLit -> lit
convertLit (Loc lit loc) =
    case lit of
        BoolL b   -> boolL b loc
        NumL n    -> numL n loc
        StringL s -> stringL s loc

convertVar :: IsVar var => LocVar -> var
convertVar (Loc var loc) =
    case var of
        NamedVar n -> namedVar n loc
        RxCapVar i -> rxCapVar i loc

convertExpr :: IsExpr expr => LocExpr -> expr
convertExpr (Loc e loc) =
    case e of
        LitE lit  -> litE (convertLit lit) loc
        VarE var  -> varE (convertVar var) loc
        AppE f x  -> appE f (convertExpr x) loc
        PlusE x y -> plusE (convertExpr x) (convertExpr y) loc
        MultE x y -> multE (convertExpr x) (convertExpr y) loc
        NegE x    -> negE (convertExpr x) loc
        InterpE i -> interpE (map (fmap convertVar) i) loc

convertPred :: IsPred pred => LocPred -> pred
convertPred (Loc p loc) =
    case p of
        ScopeP p1     -> scopeP (convertPred p1) loc
        NotP p1       -> notP (convertPred p1) loc
        AndP p1 p2    -> andP (convertPred p1) (convertPred p2) loc
        OrP p1 p2     -> orP (convertPred p1) (convertPred p2) loc
        ExprP e       -> exprP (convertExpr e) loc
        MatchP e rx m -> matchP (convertExpr e) rx m loc
        OpP op e1 e2  -> opP op (convertExpr e1) (convertExpr e2) loc
