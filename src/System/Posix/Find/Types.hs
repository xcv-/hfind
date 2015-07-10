{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module System.Posix.Find.Types where

import Data.Bifunctor
import Data.Typeable (Typeable)

import Control.Monad.Catch (Exception)

import Pipes
import qualified Pipes.Prelude as P

import Path
import qualified Path.Internal as PathInternal

import qualified System.FilePath as FP

import System.Posix.Files (FileStatus)


data Ls m fp dp
  = FileP fp
  | DirP  dp (Producer' (Ls m fp dp) m ())

instance Monad m => Bifunctor (Ls m) where
    bimap ffp _   (FileP fp)    = FileP (ffp fp)
    bimap ffp fdp (DirP dp mbs) = DirP (fdp dp) (mbs >-> P.map (bimap ffp fdp))


data FSNodeType = Raw | WithLinks | WithoutLinks | Resolved

type family HasLinks (s :: FSNodeType) :: Bool where
    HasLinks 'Raw       = 'True
    HasLinks 'WithLinks = 'True
    HasLinks s          = 'False

type family HasErrors (s :: FSNodeType) :: Bool where
    HasErrors 'Resolved = 'False
    HasErrors 'Raw      = 'False
    HasErrors s         = 'True


newtype Link = Link { getLinkPath :: Path Abs File }
    deriving (Eq)

instance Show Link where
    show (Link p) = show p


data FSNode t (s :: FSNodeType) where
    FileNode ::                        FileStatus -> Path Abs File      -> FSNode File s
    DirNode  ::                        FileStatus -> Path Abs Dir       -> FSNode Dir  s
    SymLink  :: HasLinks  s ~ 'True => FileStatus -> Link -> FSNode t s -> FSNode t    s
    Missing  :: HasErrors s ~ 'True => FilePath                         -> FSNode t    s
    FSCycle  :: HasErrors s ~ 'True => Link                             -> FSNode t    s

nodePath :: (HasErrors s ~ 'False, HasLinks s ~ 'False)
         => FSNode t s -> Path Abs t
nodePath (FileNode _ p) = p
nodePath (DirNode _ p)  = p
nodePath _ =
    error "System.Posix.Find.Types.nodePath: the impossible just happened!"


instance Show (FSNode t l) where
    show (FileNode _ p)  = "File      " ++ show p
    show (DirNode _ p)   = "Directory " ++ show p
    show (SymLink _ s p) = "SymLink   " ++ show s ++ " -> " ++ show p
    show (Missing p)     = "Missing   " ++ show p
    show (FSCycle l)     = "Cycle     " ++ show l

data ListEntry fp dp = FileEntry fp | DirEntry dp

instance (Show fp, Show dp) => Show (ListEntry fp dp) where
    show (DirEntry  p) = show p
    show (FileEntry p) = show p


class IsPathType t where
    asFilePath :: Path Abs t -> Path Abs File
    asDirPath  :: Path Abs t -> Path Abs Dir

instance IsPathType File where
    asFilePath = id
    asDirPath  = PathInternal.Path . FP.addTrailingPathSeparator . toFilePath

instance IsPathType Dir where
    asFilePath = PathInternal.Path . FP.dropTrailingPathSeparator . toFilePath
    asDirPath = id


type LsN  m l = Ls m (FSNode File l) (FSNode Dir l)
type LsL  m   = LsN m 'WithLinks
type LsN' m   = LsN m 'WithoutLinks
type LsR  m   = LsN m 'Resolved


newtype FSCycleError = FSCycleError Link
  deriving (Typeable)

instance Show FSCycleError where
    show (FSCycleError (Link p)) = "File system cycle at " ++ show p

instance Exception FSCycleError


newtype FileNotFoundError = FileNotFoundError FilePath
  deriving (Typeable)

instance Show FileNotFoundError where
    show (FileNotFoundError p) = "File not found: " ++ p

instance Exception FileNotFoundError
