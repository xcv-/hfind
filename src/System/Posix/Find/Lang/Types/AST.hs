{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module System.Posix.Find.Lang.Types.AST where

import Data.Int      (Int64)
import Data.Text     (Text)
import Data.Text.ICU (Regex)


-- enable both AST debugging and fusing parsing/analysis (see Eval.hs)

class IsLit lit where
    boolL   :: Bool   -> lit
    numL    :: Int64  -> lit
    stringL :: Text -> lit

class IsVar var where
    namedVar :: Text -> var
    rxCapVar :: Int    -> var

class (IsLit (ExprLit expr), IsVar (ExprVar expr)) => IsExpr expr where
    type ExprLit expr
    type ExprVar expr

    litE    :: ExprLit expr -> expr
    varE    :: ExprVar expr -> expr
    appE    :: Name -> expr -> expr

    interpE :: [Either Text (ExprVar expr)] -> expr

class IsExpr (PredExpr pre) => IsPred pre where
    type PredExpr pre

    notP  :: pre        -> pre
    andP  :: pre -> pre -> pre
    orP   :: pre -> pre -> pre

    exprP  :: PredExpr pre -> pre
    matchP :: PredExpr pre -> Regex -> RxCaptureMode -> pre
    opP    :: Op -> PredExpr pre -> PredExpr pre -> pre



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
          | InterpE ![Either Text Var]
    deriving (Show)


data Op = OpEQ | OpLT | OpLE | OpGT | OpGE
    deriving (Eq, Show)

data RxCaptureMode = Capture | NoCapture
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
