{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module System.Posix.Find.Walk
    ( SymlinkStrategy
    , followSymlinks
    , symlinksAreFiles
    , getStat
    , walk
    , walk1
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.List (sort)

import qualified Data.ByteString    as B

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Pipes
import qualified Pipes.Prelude as P

import System.Directory

import qualified System.Posix.Types            as Posix
import qualified System.Posix.Files.ByteString as Posix

import System.Posix.Find.Types
import System.Posix.Text.Path


newtype SymlinkStrategy s = SymlinkStrategy
    { discriminateLink :: [Posix.FileID]
                       -> Posix.FileStatus
                       -> (B.ByteString, T.Text, Path Abs File)
                       -> IO (Maybe ([Posix.FileID], NodeListEntry s))
    }


followSymlinks :: SymlinkStrategy 'WithLinks
followSymlinks = SymlinkStrategy discriminate
  where
    discriminate visited stat (bpath, tpath, path)
      | Posix.isSymbolicLink stat = do
          let inode = Posix.fileID stat

          (visited', target) <- do
              when (inode `elem` visited) $
                  throwM (FSCycleError tpath)

              tdest <- T.decodeUtf8 <$> Posix.readSymbolicLink bpath

              case canonicalizeBeside path tdest of
                  Just p'  -> discriminatePath followSymlinks (inode:visited) p'
                  Nothing  -> throwM (FileNotFoundError tdest)

          let link = Link path

          return . Just $
            case target of
                FileEntry file -> (visited', FileEntry (Symlink stat link file))
                DirEntry  dir  -> (visited', DirEntry  (Symlink stat link dir))

      | otherwise = return Nothing


symlinksAreFiles :: SymlinkStrategy 'WithoutLinks
symlinksAreFiles = SymlinkStrategy discriminate
  where
    discriminate _ _ _ = return Nothing


discriminatePath :: SymlinkStrategy s
                 -> [Posix.FileID]
                 -> Path Abs File
                 -> IO ([Posix.FileID], NodeListEntry s)
discriminatePath strat = go
  where
    go visited path = do
      let bpath = toByteString path
          tpath = toText path

      stat <- Posix.getSymbolicLinkStatus bpath
                `catchAll` \_ -> throwM (FileNotFoundError tpath)

      discr <- discriminateLink strat visited stat (bpath, tpath, path)

      let inode    = Posix.fileID stat
          visited' = inode:visited

      return $
        case discr of
            Just n -> n

            Nothing
              | Posix.isDirectory stat ->
                  (visited', DirEntry  (DirNode  stat (asDirPath  path)))
              | otherwise ->
                  (visited', FileEntry (FileNode stat (asFilePath path)))


getStat :: forall s. HasErrors s ~ 'True
        => Path Abs File -> SymlinkStrategy s -> IO (FSAnyNode s)
getStat path strat = handling $ do
    (_, entry) <- discriminatePath strat [] path

    case entry of
        FileEntry file -> return (AnyNode file)
        DirEntry dir   -> return (AnyNode dir)
  where
    handling :: IO (FSAnyNode s) -> IO (FSAnyNode s)
    handling = handle (\(FSCycleError l)      -> return $! AnyNode (FSCycle l))
             . handle (\(FileNotFoundError p) -> return $! AnyNode (Missing p))


walk :: (MonadIO m, MonadIO io, MonadCatch m, HasErrors s ~ 'True)
     => SymlinkStrategy s
     -> Path Abs Dir
     -> Producer' (WalkN m s) io ()
walk strat root =
    yield =<< liftIO (walk1 strat root)


walk1 :: forall m s.  (MonadIO m, MonadCatch m, HasErrors s ~ 'True)
      => SymlinkStrategy s
      -> Path Abs Dir
      -> IO (WalkN m s)
walk1 strat = liftIO . go []
  where
    go visited root = do
        (visited', entry) <- discriminatePath strat visited (asFilePath root)

        return $
          case entry of
            FileEntry file ->
                FileP file

            DirEntry dir ->
                DirP dir $ do
                  contents <- liftIO $ getDirectoryContents (toString root)

                  each (sort $ filter valid contents)
                    >-> P.map  (unsafeRelDir . addTrailingSlash . T.pack)
                    >-> P.map  (root </>)
                    >-> P.mapM (liftIO . handling . go visited')

    valid :: FilePath -> Bool
    valid ""   = False
    valid "."  = False
    valid ".." = False
    valid _    = True

    handling :: IO (WalkN m s) -> IO (WalkN m s)
    handling = handle (\(FSCycleError l)      -> return (FileP (FSCycle l)))
             . handle (\(FileNotFoundError p) -> return (FileP (Missing p)))
