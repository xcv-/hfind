{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module System.Posix.Find.Lang.Builtins
    ( builtinVars
    , lookupBuiltin
    ) where

import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T

import System.Posix.Find.Types
import System.Posix.Find.Lang.Types


newtype NodeFunc' = WrapForall
    { unwrap :: forall m. Monad m => NodeFunc m }

wrap :: (forall m t. Monad m => FSNode t 'Resolved -> m Lit) -> NodeFunc'
wrap p = WrapForall (NodeFunc p)


builtinVars :: [T.Text]
builtinVars = H.keys builtins

lookupBuiltin :: Monad m => T.Text -> Maybe (NodeFunc m)
lookupBuiltin name = fmap unwrap (H.lookup name builtins)

builtins :: H.HashMap T.Text NodeFunc'
builtins = H.fromList
    [ ("type",    wrap var_type)
    , ("hidden",  wrap var_hidden)
    , ("name",    wrap var_name)
    , ("path",    wrap var_path)
    , ("relpath", wrap var_relpath)
    , ("parent",  wrap var_parent)
    , ("size",    wrap var_size)
    , ("perms",   wrap var_perms)
    , ("owner",   wrap var_owner)
    , ("ownerid", wrap var_ownerid)
    , ("group",   wrap var_group)
    , ("groupid", wrap var_groupid)
    ]
  where
    var_type    = undefined
    var_hidden  = undefined
    var_name    = undefined
    var_path    = undefined
    var_relpath = undefined
    var_parent  = undefined
    var_size    = undefined
    var_perms   = undefined
    var_owner   = undefined
    var_ownerid = undefined
    var_group   = undefined
    var_groupid = undefined
