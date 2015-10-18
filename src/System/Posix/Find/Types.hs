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

import Data.Text (Text)

import Data.Typeable (Typeable)

import Control.Monad.Catch (Exception)

import Pipes
import qualified Pipes.Prelude as P

import System.Posix.Files (FileStatus)
import qualified System.Posix.Files as Posix

import System.Posix.Text.Path (Path, Abs, File, Dir, RawPath,
                               PathType(..), IsPathType,
                               Link(..))
import qualified System.Posix.Text.Path as Path


-- too useful to put anywhere else

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 1 <&>


-- filesystem tree

data Walk m fp dp
  = FileP fp
  | DirP  dp (Producer' (Walk m fp dp) m ())

instance Monad m => Bifunctor (Walk m) where
    bimap ffp _   (FileP fp)    = FileP (ffp fp)
    bimap ffp fdp (DirP dp mbs) = DirP (fdp dp) (mbs >-> P.map (bimap ffp fdp))

type WalkP  m   = Walk m (Path Abs File) (Path Abs Dir)
type WalkN  m s = Walk m (FSNode File s) (FSNode Dir s)
type WalkL  m   = WalkN m 'WithLinks
type WalkN' m   = WalkN m 'WithoutLinks
type WalkR  m   = WalkN m 'Resolved


data FSNodeType = Raw | WithLinks | WithoutLinks | Resolved

type family HasLinks (s :: FSNodeType) :: Bool where
    HasLinks 'Raw       = 'True
    HasLinks 'WithLinks = 'True
    HasLinks s          = 'False

type family HasErrors (s :: FSNodeType) :: Bool where
    HasErrors 'Resolved = 'False
    HasErrors 'Raw      = 'False
    HasErrors s         = 'True


data FSNode :: PathType -> FSNodeType -> * where
    FileNode ::                        FileStatus -> Path Abs File      -> FSNode File s
    DirNode  ::                        FileStatus -> Path Abs Dir       -> FSNode Dir  s
    Symlink  :: HasLinks  s ~ 'True => FileStatus -> Link -> FSNode t s -> FSNode t    s
    Missing  :: HasErrors s ~ 'True => RawPath                          -> FSNode t    s
    FSCycle  :: HasErrors s ~ 'True => RawPath                          -> FSNode t    s

instance IsPathType t => Eq (FSNode t s) where
    (==) = nodeEq

instance Show (FSNode t s) where
    show (FileNode _ p)  = "File      " ++ show p
    show (DirNode _ p)   = "Directory " ++ show p
    show (Symlink _ s p) = "Symlink   " ++ show s ++ " -> " ++ show p
    show (Missing p)     = "Missing   " ++ show p
    show (FSCycle p)     = "Cycle     " ++ show p


nodeEq :: FSNode t s -> FSNode t' s -> Bool
nodeEq (FileNode stat p) (FileNode stat' p') =
    Posix.fileID stat == Posix.fileID stat' && p == p'
nodeEq (DirNode stat p)  (DirNode stat' p') =
    Posix.fileID stat == Posix.fileID stat' && p == p'
nodeEq (Symlink stat (Link p) _) (Symlink stat' (Link p') _) =
    Posix.fileID stat == Posix.fileID stat' && p == p'
nodeEq _ _ =
    False


nodePath :: (HasErrors s ~ 'False, HasLinks s ~ 'False)
         => FSNode t s -> Path Abs t
nodePath (FileNode _ p) = p
nodePath (DirNode _ p)  = p
nodePath _ =
    error "System.Posix.Find.Types.nodePath: the impossible just happened!"

nodeStat :: (HasErrors s ~ 'False, HasLinks s ~ 'False)
         => FSNode t s -> FileStatus
nodeStat (FileNode stat _) = stat
nodeStat (DirNode  stat _) = stat
nodeStat _ =
    error "System.Posix.Find.Types.nodeStat: the impossible just happened!"


data FSAnyNode s = forall t. AnyNode (FSNode t s)

type FSAnyNodeR = FSAnyNode 'Resolved

instance Eq (FSAnyNode 'Resolved) where
    AnyNode n1 == AnyNode n2 = n1 `nodeEq` n2


-- listing

data ListEntry fp dp = FileEntry fp | DirEntry dp

type PathListEntry   = ListEntry (Path Abs File) (Path Abs Dir)
type NodeListEntry s = ListEntry (FSNode File s) (FSNode Dir s)
type NodeListEntryR  = NodeListEntry 'Resolved

entryRawPath :: NodeListEntryR -> Text
entryRawPath (FileEntry fp) = Path.toText (nodePath fp)
entryRawPath (DirEntry dp)  = Path.toText (nodePath dp)

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
