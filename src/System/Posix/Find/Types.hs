{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module System.Posix.Find.Types where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Data.Typeable (Typeable)

import Control.Monad.Catch (Exception)

import Pipes
import qualified Pipes.Prelude as P

import System.Posix.Files (FileStatus)

import System.Posix.Text.Path


-- filesystem tree

data Ls m fp dp
  = FileP fp
  | DirP  dp (Producer' (Ls m fp dp) m ())

instance Monad m => Bifunctor (Ls m) where
    bimap ffp _   (FileP fp)    = FileP (ffp fp)
    bimap ffp fdp (DirP dp mbs) = DirP (fdp dp) (mbs >-> P.map (bimap ffp fdp))

type LsN  m l = Ls m (FSNode File l) (FSNode Dir l)
type LsL  m   = LsN m 'WithLinks
type LsN' m   = LsN m 'WithoutLinks
type LsR  m   = LsN m 'Resolved


data FSNodeType = Raw | WithLinks | WithoutLinks | Resolved

type family HasLinks (s :: FSNodeType) :: Bool where
    HasLinks 'Raw       = 'True
    HasLinks 'WithLinks = 'True
    HasLinks s          = 'False

type family HasErrors (s :: FSNodeType) :: Bool where
    HasErrors 'Resolved = 'False
    HasErrors 'Raw      = 'False
    HasErrors s         = 'True


data FSNode :: PathDest -> FSNodeType -> * where
    FileNode ::                        FileStatus -> Path Abs File      -> FSNode File s
    DirNode  ::                        FileStatus -> Path Abs Dir       -> FSNode Dir  s
    SymLink  :: HasLinks  s ~ 'True => FileStatus -> Link -> FSNode t s -> FSNode t    s
    Missing  :: HasErrors s ~ 'True => RawPath                          -> FSNode t    s
    FSCycle  :: HasErrors s ~ 'True => RawPath                          -> FSNode t    s

instance Show (FSNode t l) where
    show (FileNode _ p)  = "File      " ++ show p
    show (DirNode _ p)   = "Directory " ++ show p
    show (SymLink _ s p) = "SymLink   " ++ show s ++ " -> " ++ show p
    show (Missing p)     = "Missing   " ++ show p
    show (FSCycle p)     = "Cycle     " ++ show p


nodePath :: (HasErrors s ~ 'False, HasLinks s ~ 'False)
         => FSNode t s -> Path Abs t
nodePath (FileNode _ p) = p
nodePath (DirNode _ p)  = p
nodePath _ =
    error "System.Posix.Find.Types.nodePath: the impossible just happened!"


-- listing

data ListEntry fp dp = FileEntry fp | DirEntry dp

type PathListEntry   = ListEntry (Path Abs File) (Path Abs Dir)
type NodeListEntry s = ListEntry (FSNode File s) (FSNode Dir s)

instance (Show fp, Show dp) => Show (ListEntry fp dp) where
    show (DirEntry  p) = show p
    show (FileEntry p) = show p

instance Bifunctor ListEntry where
    bimap f _ (FileEntry fp) = FileEntry (f fp)
    bimap _ g (DirEntry dp)  = DirEntry  (g dp)

instance Bifoldable ListEntry where
    bifoldr f _ acc (FileEntry fp) = f fp acc
    bifoldr _ g acc (DirEntry  dp) = g dp acc

instance Bitraversable ListEntry where
    bitraverse f _ (FileEntry fp) = FileEntry <$> f fp
    bitraverse _ g (DirEntry  dp) = DirEntry  <$> g dp



-- errors

newtype FSCycleError = FSCycleError RawPath
  deriving (Typeable)

instance Show FSCycleError where
    show (FSCycleError p) = "File system cycle at " ++ show p

instance Exception FSCycleError


newtype FileNotFoundError = FileNotFoundError RawPath
  deriving (Typeable)

instance Show FileNotFoundError where
    show (FileNotFoundError p) = "File not found: " ++ show p

instance Exception FileNotFoundError
