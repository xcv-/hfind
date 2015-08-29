{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module System.Posix.Find.Lang.Types.Value where

import Data.Int  (Int64)

import Data.Text (Text)
import qualified Data.Text as T

import qualified System.Posix.Text.Path as Path

import System.Posix.Find.Types (FSAnyNodeR, FSAnyNode(..), nodePath)


data Value = BoolV   !Bool
           | NumV    !Int64
           | StringV !Text
           | NodeV   !FSAnyNodeR
    deriving (Eq)

data ValueType = TBool | TNum | TString | TNode
    deriving (Eq, Show)


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

instance IsValue Text where
    valueTypeOf _ = TString

    toValue = StringV

    fromValue (StringV s) = Just s
    fromValue _           = Nothing

instance IsValue FSAnyNodeR where
    valueTypeOf _ = TNode

    toValue = NodeV

    fromValue (NodeV n) = Just n
    fromValue _         = Nothing



typeOf :: Value -> ValueType
typeOf (BoolV _)   = TBool
typeOf (NumV _)    = TNum
typeOf (StringV _) = TString
typeOf (NodeV _)   = TNode


typeName :: ValueType -> Text
typeName TBool   = "boolean"
typeName TNum    = "number"
typeName TString = "string"
typeName TNode   = "fsnode"


typeNameOf :: Value -> Text
typeNameOf = typeName . typeOf


coerceToString :: Value -> Text
coerceToString = \case
    BoolV   True        -> "true"
    BoolV   False       -> "false"
    NumV    x           -> T.pack (show x)
    StringV s           -> s
    NodeV   (AnyNode n) -> Path.toText (nodePath n)
