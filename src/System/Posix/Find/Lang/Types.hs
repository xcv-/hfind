{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module System.Posix.Find.Lang.Types where

import Data.Int (Int64)
import Data.Vector.Mutable (IOVector)

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
    regexL  :: Regex  -> RxCaptureMode -> lit

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

    exprP :: PredExpr pre -> pre
    opP   :: Op -> PredExpr pre -> PredExpr pre -> pre


-- predicate AST

data ExprType = TBool | TNum | TString | TRegex
    deriving (Eq, Show)

data RxCaptureMode = Capture | NoCapture
    deriving (Eq, Show)

data Lit = BoolL   !Bool
         | NumL    !Int64
         | StringL !T.Text
         | RegexL  !Regex  !RxCaptureMode
    deriving (Show)

typeOf :: Lit -> ExprType
typeOf (BoolL _)    = TBool
typeOf (NumL _)     = TNum
typeOf (StringL _)  = TString
typeOf (RegexL _ _) = TRegex

typeName :: ExprType -> T.Text
typeName TBool   = T.pack "boolean"
typeName TNum    = T.pack "number"
typeName TString = T.pack "string"
typeName TRegex  = T.pack "regex"

typeNameOf :: Lit -> T.Text
typeNameOf = typeName . typeOf


data Var = NamedVar !T.Text
         | RxCapVar !Int
    deriving (Eq, Show)

data Expr = LitE    !Lit
          | VarE    !Var
          | InterpE ![Either T.Text Var]
    deriving (Show)


data Op = OpEQ | OpLT | OpLE | OpGT | OpGE | OpRX
    deriving (Eq, Show)


data Pred = NotP  !Pred
          | AndP  !Pred !Pred
          | OrP   !Pred !Pred
          | ExprP !Expr
          | OpP   !Op !Expr !Expr
    deriving Show



instance IsLit Lit where
    boolL   = BoolL
    numL    = NumL
    stringL = StringL
    regexL  = RegexL

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

    exprP = ExprP
    opP   = OpP


-- pre-evaluated predicates

newtype NodeFunc m = NodeFunc
    { evalNodeFunc :: forall t. FSNode t 'Resolved -> m Lit }

newtype FilePredicate = FilePredicate
    { evalFilePredicate :: FSNode 'File 'Resolved -> Bool }

newtype DirPredicate m = DirPredicate
    { evalDirPredicate :: FSNode 'Dir 'Resolved -> m Bool }

newtype NodePredicate m = NodePredicate
    { evalNodePredicate :: forall t. FSNode t 'Resolved -> m Bool }

newtype EntryPredicate m = EntryPredicate
    { evalEntryPredicate :: NodeListEntry 'Resolved -> m Bool }


-- preprocessing/evaluation contexts

type VarId   = Int
type VarName = T.Text

data ScanningContext = ScanningContext
  { ctxVars           :: ![VarName] -- reversed
  , ctxNumVars        :: !Int
  , ctxNumCaptures    :: !Int
  , ctxMaxNumVars     :: !Int
  , ctxMaxNumCaptures :: !Int
  }

data EvalContext = EvalContext
  { ctxValues      :: !(IOVector Lit) -- reversed
  , ctxActiveRegex :: !(Maybe Regex)  -- not reversed
  }


-- TODO: error location info

data VarNotFoundError = VarNotFound !Var

data TypeError = ExpectedButFound !T.Text !T.Text

expectedButFound :: ExprType -> ExprType -> TypeError
t `expectedButFound` t' = typeName t `ExpectedButFound` typeName t'


type Evaluating n =
      (MonadIO n, MonadError TypeError n, MonadState EvalContext n)

type Scanning m =
      (MonadIO m, MonadError VarNotFoundError m, MonadState ScanningContext m)

type ScanningFor m n = (Scanning m, Evaluating n)
