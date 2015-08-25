{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module System.Posix.Find.Lang.Builtins
    ( Builtins
    , BuiltinVar
    , BuiltinFunc
    , reservedNames
    , reservedVars
    , reservedFuncs
    , lookupVar
    , lookupFunc
    , mkBuiltins
    ) where

import Control.Monad.IO.Class
import Control.Monad.Except

import Data.Int (Int64)

import qualified Data.Text as T

import qualified Data.HashMap.Strict as H

import qualified System.Posix as Posix

import System.Posix.Text.Path (Path, Abs, File, RawPath)
import qualified System.Posix.Text.Path as Path

import System.Posix.Find.Types
import System.Posix.Find.Lang.Types
import System.Posix.Find.Ls


-- m is usually EvalT IO

type BuiltinVar  m = FSAnyNode 'Resolved -> m Value
type BuiltinFunc m = Value               -> m Value

data Builtins m = Builtins
    { builtinVars  :: H.HashMap Name (BuiltinVar  m)
    , builtinFuncs :: H.HashMap Name (BuiltinFunc m)
    }

reservedNames :: Builtins m -> [Name]
reservedNames builtins = reservedVars builtins ++ reservedFuncs builtins

reservedVars :: Builtins m -> [Name]
reservedVars = H.keys . builtinVars

reservedFuncs :: Builtins m -> [Name]
reservedFuncs = H.keys . builtinFuncs


lookupVar :: Builtins m -> Name -> Maybe (BuiltinVar m)
lookupVar builtins name = H.lookup name (builtinVars builtins)

lookupFunc :: Builtins m -> Name -> Maybe (BuiltinFunc m)
lookupFunc builtins name = H.lookup name (builtinFuncs builtins)


mkBuiltins :: forall m. (MonadError RuntimeError m, MonadIO m)
           => Path.Path Path.Abs Path.Dir -> Builtins m
mkBuiltins root =
    Builtins vars funcs
  where
    erase :: (IsValue a, IsValue b) => (a -> m b) -> BuiltinFunc m
    erase f val =
        case fromValue val of
            Just x  -> fmap toValue (f x)
            nothing -> throwError $
                valueTypeOf nothing `expectedButFound` typeOf val

    vars = H.fromList []

    funcs = H.fromList
        [ ("stat",        erase fn_stat)
        , ("exists",      erase fn_exists)
        , ("isfile",      erase fn_isfile)
        , ("isdir",       erase fn_isdir)
        , ("islink",      erase fn_islink)
        , ("type",        erase fn_type)
        , ("hidden",      erase fn_hidden)
        , ("name",        erase fn_name)
        , ("path",        erase fn_path)
        , ("relpath",     erase fn_relpath)
        , ("parent",      erase fn_parent)
        , ("parentpath",  erase fn_parentpath)
        , ("parentname",  erase fn_parentname)
        , ("size",        erase fn_size)
        , ("nlinks",      erase fn_nlinks)
        , ("perms",       erase fn_perms)
        , ("owner",       erase fn_owner)
        , ("ownerid",     erase fn_ownerid)
        , ("group",       erase fn_group)
        , ("groupid",     erase fn_groupid)
        ]


    canonicalize :: RawPath -> m (Path Abs File)
    canonicalize "" = throwError (InvalidPathOp "" "canonicalize")
    canonicalize p =
        case Path.canonicalizeUnder root p of
            Just path -> return path
            Nothing   -> throwError (InvalidPathOp p "canonicalize")


    fn_stat :: T.Text -> m (FSAnyNode 'Resolved)
    fn_stat ""  = throwError (InvalidPathOp "" "canonicalize")
    fn_stat raw = do
        path <- canonicalize raw

        n <- liftIO $ loadNodeAt path symlinksAreFiles

        case n of
            AnyNode (FileNode st p) -> return $ AnyNode (FileNode st p)
            AnyNode (DirNode st p)  -> return $ AnyNode (DirNode st p)
            AnyNode (Missing p)     -> throwError (NotFound p)
            _                       -> error "fn_stat: impossible!"

    falseWhenNotFound :: m Bool -> m Bool
    falseWhenNotFound m = m `catchError` \case
        NotFound      _   -> return False
        InvalidPathOp _ _ -> return False
        e                 -> throwError e


    fn_exists :: T.Text -> m Bool
    fn_exists raw = falseWhenNotFound $
        fmap (const True)
             (fn_stat raw)

    fn_isfile :: T.Text -> m Bool
    fn_isfile raw = falseWhenNotFound $
        fmap (\(AnyNode n) -> Posix.isRegularFile (nodeStat n))
             (fn_stat raw)

    fn_isdir :: T.Text -> m Bool
    fn_isdir raw = falseWhenNotFound $
        fmap (\(AnyNode n) -> Posix.isDirectory (nodeStat n))
             (fn_stat raw)

    fn_islink :: T.Text -> m Bool
    fn_islink raw = falseWhenNotFound $
        fmap (\(AnyNode n) -> Posix.isSymbolicLink (nodeStat n))
             (fn_stat raw)


    fn_type :: FSAnyNode 'Resolved -> m T.Text
    fn_type (AnyNode n)
      | Posix.isBlockDevice     s = return "b"
      | Posix.isCharacterDevice s = return "c"
      | Posix.isDirectory       s = return "d"
      | Posix.isNamedPipe       s = return "p"
      | Posix.isRegularFile     s = return "f"
      | Posix.isSocket          s = return "s"
      | Posix.isSymbolicLink    s = return "l"
      | otherwise                 = return "?"
      where
        s = nodeStat n

    fn_hidden :: FSAnyNode 'Resolved -> m Bool
    fn_hidden n = do
         name <- fn_name n
         return ("." `T.isPrefixOf` name)

    fn_name :: FSAnyNode 'Resolved -> m T.Text
    fn_name (AnyNode n) =
        let path = nodePath n in
        case Path.filename path of
            Just name -> return name
            Nothing   -> throwError (InvalidPathOp (Path.toText path) "filename")

    fn_path :: FSAnyNode 'Resolved -> m T.Text
    fn_path (AnyNode n) =
        return $! Path.toText (nodePath n)

    fn_relpath :: FSAnyNode 'Resolved -> m T.Text
    fn_relpath (AnyNode _) =
        undefined

    fn_parent :: FSAnyNode 'Resolved -> m (FSAnyNode 'Resolved)
    fn_parent n = do
        parentPath <- fn_parentpath n
        fn_stat parentPath

    fn_parentpath :: FSAnyNode 'Resolved -> m T.Text
    fn_parentpath (AnyNode n) =
        let path = nodePath n in
        case Path.parent path of
            Just parent -> return (Path.toText (Path.asDirPath parent))
            Nothing     -> throwError (InvalidPathOp (Path.toText path) "parent")

    fn_parentname :: FSAnyNode 'Resolved -> m T.Text
    fn_parentname = fn_parent >=> fn_name

    fn_size :: FSAnyNode 'Resolved -> m Int64
    fn_size (AnyNode n) =
        return $! fromIntegral (Posix.fileSize (nodeStat n))

    fn_nlinks :: FSAnyNode 'Resolved -> m Int64
    fn_nlinks (AnyNode n) =
        return $! fromIntegral (Posix.linkCount (nodeStat n))

    fn_perms :: FSAnyNode 'Resolved -> m T.Text
    fn_perms (AnyNode n) =
        return $! T.pack
          [ if has Posix.ownerReadMode    then 'r' else '-'
          , if has Posix.ownerWriteMode   then 'w' else '-'
          , if has Posix.ownerExecuteMode then 'x' else '-'
          , if has Posix.groupReadMode    then 'r' else '-'
          , if has Posix.groupWriteMode   then 'w' else '-'
          , if has Posix.groupExecuteMode then 'x' else '-'
          , if has Posix.otherReadMode    then 'r' else '-'
          , if has Posix.otherWriteMode   then 'w' else '-'
          , if has Posix.otherExecuteMode then 'x' else '-'
          ]
      where
        has perm = Posix.intersectFileModes mode perm == perm
        mode = Posix.fileMode (nodeStat n)

    fn_ownerid  :: FSAnyNode 'Resolved -> m Int64
    fn_ownerid (AnyNode n) =
        return $! fromIntegral (Posix.fileOwner (nodeStat n))

    fn_owner :: FSAnyNode 'Resolved -> m T.Text
    fn_owner n = do
        ownerId    <- fn_ownerid n
        ownerEntry <- liftIO $ Posix.getUserEntryForID (fromIntegral ownerId)
        return $! T.pack (Posix.userName ownerEntry)

    fn_groupid :: FSAnyNode 'Resolved -> m Int64
    fn_groupid (AnyNode n) =
        return $! fromIntegral (Posix.fileGroup (nodeStat n))

    fn_group :: FSAnyNode 'Resolved -> m T.Text
    fn_group n = do
        groupId    <- fn_groupid n
        groupEntry <- liftIO $ Posix.getGroupEntryForID (fromIntegral groupId)
        return $! T.pack (Posix.groupName groupEntry)
