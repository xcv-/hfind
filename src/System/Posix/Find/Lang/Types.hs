{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module System.Posix.Find.Lang.Types where

import Data.Int (Int64)

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Text as T

import Data.Text.ICU.Regex (Regex)

import System.Posix.Find.Types
import System.Posix.Text.Path


-- enable both AST debugging and fusing parsing/analysis (see Eval.hs)

class IsLit lit where
    boolL   :: Bool   -> lit
    numL    :: Int64  -> lit
    stringL :: T.Text -> lit

class IsVar var where
    namedVar :: T.Text -> var
    rxCapVar :: Int    -> var

class (IsLit (ExprLit expr), IsVar (ExprVar expr)) => IsExpr expr where
    type ExprLit expr
    type ExprVar expr

    litE    :: ExprLit expr                   -> expr
    varE    :: ExprVar expr                   -> expr
    interpE :: [Either T.Text (ExprVar expr)] -> expr

class IsExpr (PredExpr pre) => IsPred pre where
    type PredExpr pre

    notP  :: pre        -> pre
    andP  :: pre -> pre -> pre
    orP   :: pre -> pre -> pre

    exprP  :: PredExpr pre -> pre
    matchP :: PredExpr pre -> Regex -> RxCaptureMode -> pre
    opP    :: Op -> PredExpr pre -> PredExpr pre -> pre


-- predicate AST

data Value = BoolV   !Bool
           | NumV    !Int64
           | StringV !T.Text
           | forall t. IsPathType t => NodeV !(FSNode t 'Resolved)

data ValueType = TBool | TNum | TString | TNode
    deriving (Eq, Show)

data RxCaptureMode = Capture | NoCapture
    deriving (Eq, Show)

data Lit = BoolL   !Bool
         | NumL    !Int64
         | StringL !T.Text
    deriving (Show)

typeOf :: Value -> ValueType
typeOf (BoolV _)   = TBool
typeOf (NumV _)    = TNum
typeOf (StringV _) = TString
typeOf (NodeV _)   = TNode

typeName :: ValueType -> T.Text
typeName TBool   = T.pack "boolean"
typeName TNum    = T.pack "number"
typeName TString = T.pack "string"
typeName TNode   = T.pack "fsnode"

typeNameOf :: Value -> T.Text
typeNameOf = typeName . typeOf


data Var = NamedVar !T.Text
         | RxCapVar !Int
    deriving (Eq, Show)

data Expr = LitE    !Lit
          | VarE    !Var
          | InterpE ![Either T.Text Var]
    deriving (Show)


data Op = OpEQ | OpLT | OpLE | OpGT | OpGE
    deriving (Eq, Show)


data Pred = NotP   !Pred
          | AndP   !Pred !Pred
          | OrP    !Pred !Pred
          | ExprP  !Expr
          | MatchP !Expr !Regex !RxCaptureMode
          | OpP    !Op !Expr !Expr
    deriving Show



instance IsLit Lit where
    boolL   = BoolL
    numL    = NumL
    stringL = StringL

instance IsVar Var where
    namedVar = NamedVar
    rxCapVar = RxCapVar

instance IsExpr Expr where
    type ExprLit Expr = Lit
    type ExprVar Expr = Var

    litE    = LitE
    varE    = VarE
    interpE = InterpE

instance IsPred Pred where
    type PredExpr Pred = Expr

    notP  = NotP
    andP  = AndP
    orP   = OrP

    exprP  = ExprP
    matchP = MatchP
    opP    = OpP

