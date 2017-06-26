{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module System.HFind.Expr.Types.Value
    ( Value(..)
    , ErasedValue(..)
    , ValueType(..)
    , IsValue(..)
    , typeName
    , eraseType
    , valueToString
    , anyValueToString
    ) where

import Data.Int  (Int64)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Typeable (Typeable)

import qualified System.HFind.Path as Path

import System.HFind.Types (FSAnyNodeR, FSAnyNode(..), nodePath)


data Value a where
    BoolV   :: !Bool       -> Value Bool
    IntV    :: !Int64      -> Value Int64
    StringV :: !Text       -> Value Text
    NodeV   :: !FSAnyNodeR -> Value FSAnyNodeR

instance Eq (Value t) where
    BoolV   a == BoolV   b = a == b
    IntV    a == IntV    b = a == b
    StringV a == StringV b = a == b
    NodeV   a == NodeV   b = a == b


data ErasedValue where
    ErasedValue :: !(Value a) -> ErasedValue

data ValueType = TBool | TNum | TString | TNode
    deriving (Eq, Show)

eraseType :: ValueType -> (forall a. Value a) -> ErasedValue
eraseType ty val =
    case ty of
        TBool   -> ErasedValue (val @Bool)
        TNum    -> ErasedValue (val @Int64)
        TString -> ErasedValue (val @Text)
        TNode   -> ErasedValue (val @FSAnyNodeR)

class Typeable a => IsValue a where
    valueTypeOf :: proxy a -> ValueType
    toValue     :: a -> Value a
    fromValue   :: Value a -> a

instance IsValue Bool where
    valueTypeOf _ = TBool
    toValue = BoolV
    fromValue (BoolV b) = b
    {-# INLINE valueTypeOf #-}
    {-# INLINE toValue #-}
    {-# INLINE fromValue #-}

instance IsValue Int64 where
    valueTypeOf _ = TNum
    toValue = IntV
    fromValue (IntV n) = n
    {-# INLINE valueTypeOf #-}
    {-# INLINE toValue #-}
    {-# INLINE fromValue #-}

instance IsValue Text where
    valueTypeOf _ = TString
    toValue = StringV
    fromValue (StringV s) = s
    {-# INLINE valueTypeOf #-}
    {-# INLINE toValue #-}
    {-# INLINE fromValue #-}

instance IsValue FSAnyNodeR where
    valueTypeOf _ = TNode
    toValue = NodeV
    fromValue (NodeV n) = n
    {-# INLINE valueTypeOf #-}
    {-# INLINE toValue #-}
    {-# INLINE fromValue #-}


typeName :: ValueType -> Text
typeName TBool   = "boolean"
typeName TNum    = "number"
typeName TString = "string"
typeName TNode   = "fsnode"

valueToString :: Value a -> Text
valueToString = \case
    BoolV   True        -> "true"
    BoolV   False       -> "false"
    IntV    x           -> T.pack (show x)
    StringV s           -> s
    NodeV   (AnyNode n) -> Path.toText (nodePath n)

anyValueToString :: ErasedValue -> Text
anyValueToString (ErasedValue v) = valueToString v
