{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module System.Posix.Find.Lang.Builtins where

import Control.Monad.IO.Class

import qualified Data.Text           as T

import qualified System.Posix.Text.Path as Path

import System.Posix.Find.Types
import System.Posix.Find.Lang.Types


type Builtin = forall t. FSNode t 'Resolved -> IO Value

var_type      :: Builtin
var_type    _ = undefined

var_hidden    :: Builtin
var_hidden  n = do StringV name <- var_name n
                   return $ BoolV ("." `T.isPrefixOf` name)

var_name      :: Builtin
var_name    n = case Path.filename (nodePath n) of
                  Just name -> return (StringV name)
                  Nothing   -> return (StringV "")

var_path      :: Builtin
var_path    n = return $ StringV (Path.toText (nodePath n))

var_relpath   :: Builtin
var_relpath _ = undefined

var_parent    :: Builtin
var_parent  _ = undefined

var_size      :: Builtin
var_size    _ = undefined

var_perms     :: Builtin
var_perms   _ = undefined

var_owner     :: Builtin
var_owner   _ = undefined

var_ownerid   :: Builtin
var_ownerid _ = undefined

var_group     :: Builtin
var_group   _ = undefined

var_groupid   :: Builtin
var_groupid _ = undefined
