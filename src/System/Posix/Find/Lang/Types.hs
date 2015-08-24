{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module System.Posix.Find.Lang.Types where

import Data.Int (Int64)

import qualified Data.Text as T

import Data.Text.ICU.Regex (Regex)

import System.Posix.Text.Path (RawPath)
import System.Posix.Find.Types


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

    litE    :: ExprLit expr -> expr
    varE    :: ExprVar expr -> expr
    appE    :: Name -> expr -> expr

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
           | NodeV   !(FSAnyNode 'Resolved)
    deriving (Eq)

data ValueType = TBool | TNum | TString | TNode
    deriving (Eq, Show)

data RxCaptureMode = Capture | NoCapture
    deriving (Eq, Show)

data Lit = BoolL   !Bool
         | NumL    !Int64
         | StringL !T.Text
    deriving (Show)


class IsValue a where
    valueTypeOf :: p a -> ValueType
    toValue     :: a -> Value
    fromValue   :: Value -> Maybe a

instance IsValue Bool where
    valueTypeOf _ = TBool

    toValue = BoolV

    fromValue (BoolV b) = Just b
    fromValue _         = Nothing

instance IsValue Int64 where
    valueTypeOf _ = TNum

    toValue = NumV

    fromValue (NumV x) = Just x
    fromValue _        = Nothing

instance IsValue T.Text where
    valueTypeOf _ = TString

    toValue = StringV

    fromValue (StringV s) = Just s
    fromValue _           = Nothing

instance IsValue (FSAnyNode 'Resolved) where
    valueTypeOf _ = TNode

    toValue = NodeV

    fromValue (NodeV n) = Just n
    fromValue _         = Nothing


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


type Name    = T.Text

data Var = NamedVar !Name
         | RxCapVar !Int
    deriving (Eq, Show)

data Expr = LitE    !Lit
          | VarE    !Var
          | AppE    !Name !Expr
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

    litE = LitE
    varE = VarE
    appE = AppE
    interpE = InterpE

instance IsPred Pred where
    type PredExpr Pred = Expr

    notP = NotP
    andP = AndP
    orP  = OrP

    exprP  = ExprP
    matchP = MatchP
    opP    = OpP


-- TODO: error location info

data VarNotFoundError = VarNotFound       !Var
                      | CaptureOutOfRange !Int
    deriving Show

data RuntimeError = !T.Text `ExpectedButFound` !T.Text
                  | NotFound !RawPath
                  | InvalidPathOp !RawPath !T.Text
    deriving Show

expectedButFound :: ValueType -> ValueType -> RuntimeError
t `expectedButFound` t' = typeName t `ExpectedButFound` typeName t'

