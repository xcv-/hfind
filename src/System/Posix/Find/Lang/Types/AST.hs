{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module System.Posix.Find.Lang.Types.AST where

import Data.Int      (Int64)
import Data.Text     (Text)
import Data.Text.ICU (Regex)

import Text.Parsec.Pos (SourcePos)


type Src = Text
type SrcLoc = (Src, SourcePos)


data Interp var = InterpLit !Text
                | InterpVar !var
    deriving (Eq, Show)


-- enable both AST debugging and fusing parsing/analysis (see Eval.hs)

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


-- AST data

type Name    = Text

data Lit = BoolL   !Bool
         | NumL    !Int64
         | StringL !Text
    deriving (Show)

data Var = NamedVar !Name
         | RxCapVar !Int
    deriving (Eq, Show)

data Expr = LitE    !Lit
          | VarE    !Var
          | AppE    !Name !Expr
          | InterpE ![Interp Var]
    deriving (Show)


data Op = OpEQ | OpLT | OpLE | OpGT | OpGE
    deriving (Eq, Show)

opSymbol :: Op -> Text
opSymbol OpEQ = "=="
opSymbol OpLT = ">"
opSymbol OpLE = ">="
opSymbol OpGT = "<"
opSymbol OpGE = "=<"


data RxCaptureMode = Capture | NoCapture
    deriving (Eq, Show)


data Pred = ScopeP !Pred
          | NotP   !Pred
          | AndP   !Pred !Pred
          | OrP    !Pred !Pred
          | ExprP  !Expr
          | MatchP !Expr !Regex !RxCaptureMode
          | OpP    !Op !Expr !Expr
    deriving Show


instance IsLit Lit where
    boolL   = const . BoolL
    numL    = const . NumL
    stringL = const . StringL

instance IsVar Var where
    namedVar = const . NamedVar
    rxCapVar = const . RxCapVar

instance IsExpr Expr where
    type ExprLit Expr = Lit
    type ExprVar Expr = Var

    litE    = const . LitE
    varE    = const . VarE
    appE f  = const . AppE f
    interpE = const . InterpE

instance IsPred Pred where
    type PredExpr Pred = Expr

    scopeP = const . ScopeP
    notP   = const . NotP
    andP p = const . AndP p
    orP p  = const . OrP p

    exprP       = const . ExprP
    matchP e rx = const . MatchP e rx
    opP op e1   = const . OpP op e1
