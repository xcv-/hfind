{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module System.HFind.Expr.Types.AST where

import Data.Int      (Int64)
import Data.Text     (Text)
import Data.Text.ICU (Regex)

import Text.Parsec.Pos (SourcePos)


type Src = Text

data SrcLoc = SrcLoc !Src !SourcePos
    deriving (Eq, Show)

type Name = Text

data Interp var = InterpLit !Text
                | InterpVar !var
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Op = OpEQ | OpLT | OpLE | OpGT | OpGE
    deriving (Eq, Show)

data RxCaptureMode = Capture | NoCapture
    deriving (Eq, Show)

opSymbol :: Op -> Text
opSymbol OpEQ = "=="
opSymbol OpLT = ">"
opSymbol OpLE = ">="
opSymbol OpGT = "<"
opSymbol OpGE = "=<"


-- enable both AST debugging and fusing parsing/analysis (see FromAST.hs)

class IsLit lit where
    boolL   :: Bool  -> SrcLoc -> lit
    numL    :: Int64 -> SrcLoc -> lit
    stringL :: Text  -> SrcLoc -> lit

class IsVar var where
    namedVar :: Text -> SrcLoc -> var
    rxCapVar :: Int  -> SrcLoc -> var

class (IsLit (ExprLit expr), IsVar (ExprVar expr)) => IsExpr expr where
    type ExprLit expr
    type ExprVar expr

    litE    :: ExprLit expr -> SrcLoc -> expr
    varE    :: ExprVar expr -> SrcLoc -> expr
    appE    :: Name -> expr -> SrcLoc -> expr

    plusE   :: expr -> expr -> SrcLoc -> expr
    multE   :: expr -> expr -> SrcLoc -> expr
    negE    :: expr -> SrcLoc -> expr

    interpE :: [Interp (ExprVar expr)] -> SrcLoc -> expr

class IsExpr (PredExpr pre) => IsPred pre where
    type PredExpr pre

    scopeP :: pre        -> SrcLoc -> pre
    notP   :: pre        -> SrcLoc -> pre
    andP   :: pre -> pre -> SrcLoc -> pre
    orP    :: pre -> pre -> SrcLoc -> pre

    exprP  :: PredExpr pre                           -> SrcLoc -> pre
    matchP :: PredExpr pre -> Regex -> RxCaptureMode -> SrcLoc -> pre
    opP    :: Op -> PredExpr pre -> PredExpr pre     -> SrcLoc -> pre



data Lit
    = BoolL   !Bool
    | NumL    !Int64
    | StringL !Text
    deriving (Eq, Show)

data Var
    = NamedVar !Name
    | RxCapVar !Int
    deriving (Eq, Show)

data Expr_ f
    = LitE    !(f Lit)
    | VarE    !(f Var)
    | AppE    !Name !(f (Expr_ f))
    | PlusE   !(f (Expr_ f)) !(f (Expr_ f))
    | MultE   !(f (Expr_ f)) !(f (Expr_ f))
    | NegE    !(f (Expr_ f))
    | InterpE ![Interp (f Var)]

data Pred_ f
    = ScopeP !(f (Pred_ f))
    | NotP   !(f (Pred_ f))
    | AndP   !(f (Pred_ f)) !(f (Pred_ f))
    | OrP    !(f (Pred_ f)) !(f (Pred_ f))
    | ExprP  !(f (Expr_ f))
    | MatchP !(f (Expr_ f)) !Regex !RxCaptureMode
    | OpP    !Op !(f (Expr_ f)) !(f (Expr_ f))


data Loc a = Loc !a !SrcLoc
    deriving (Eq, Show, Functor, Foldable, Traversable)

newtype NoLoc a = NoLoc a
    deriving (Eq, Show, Functor, Foldable, Traversable)

type Expr = Expr_ NoLoc
type Pred = Pred_ NoLoc
type LocLit  = Loc Lit
type LocVar  = Loc Var
type LocExpr = Loc (Expr_ Loc)
type LocPred = Loc (Pred_ Loc)

deriving instance
    (Show (f (Expr_ f)), Show (f Var), Show (f Lit))
        => Show (Expr_ f)
deriving instance
    (Show (f (Expr_ f)), Show (f (Pred_ f)))
        => Show (Pred_ f)


class Functor f => WithSrcLoc f where
    withLoc :: a -> SrcLoc -> f a
    withoutLoc :: f a -> a

instance WithSrcLoc Loc where
    withLoc = Loc
    withoutLoc (Loc a _) = a

instance WithSrcLoc NoLoc where
    withLoc = const . NoLoc
    withoutLoc (NoLoc a) = a


instance WithSrcLoc f => IsLit (f Lit) where
    boolL   = withLoc . BoolL
    numL    = withLoc . NumL
    stringL = withLoc . StringL

instance WithSrcLoc f => IsVar (f Var) where
    namedVar = withLoc . NamedVar
    rxCapVar = withLoc . RxCapVar

instance WithSrcLoc f => IsExpr (f (Expr_ f)) where
    type ExprLit (f (Expr_ f)) = f Lit
    type ExprVar (f (Expr_ f)) = f Var

    litE    = withLoc . LitE
    varE    = withLoc . VarE
    appE h  = withLoc . AppE h
    plusE x = withLoc . PlusE x
    multE x = withLoc . MultE x
    negE    = withLoc . NegE
    interpE = withLoc . InterpE

instance WithSrcLoc f => IsPred (f (Pred_ f)) where
    type PredExpr (f (Pred_ f)) = f (Expr_ f)

    scopeP = withLoc . ScopeP
    notP   = withLoc . NotP
    andP p = withLoc . AndP p
    orP p  = withLoc . OrP p

    exprP       = withLoc . ExprP
    matchP e rx = withLoc . MatchP e rx
    opP op e1   = withLoc . OpP op e1
