{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module System.Posix.Find.Ls where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Maybe
import Data.List (sort)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Pipes
import qualified Pipes.Prelude as P

import System.Directory

import qualified System.Posix.Types            as Posix
import qualified System.Posix.Files.ByteString as Posix

import System.Posix.Find.Types
import System.Posix.Text.Path


discriminatePath :: Bool
                 -> [Posix.FileID]
                 -> Path Abs File
                 -> IO ([Posix.FileID], NodeListEntry 'WithLinks)
discriminatePath followSymLinks = go
  where
    go visited path = do
      let bpath = toByteString path
          tpath = toText path

      stat <- Posix.getSymbolicLinkStatus bpath
                `catchAll` \_ -> throwM (FileNotFoundError tpath)

      let inode = Posix.fileID stat

      if Posix.isSymbolicLink stat && followSymLinks
        then do
          (visited', target) <- do
              when (inode `elem` visited) $ do
                  throwM (FSCycleError tpath)

              tdest <- T.decodeUtf8 <$> Posix.readSymbolicLink bpath

              case canonicalizeBeside path tdest of
                  Right p' -> go (inode:visited) p'
                  Left _   -> throwM (FileNotFoundError tdest)

          let link = Link path

          case target of
              FileEntry file -> retF visited' (SymLink stat link file)
              DirEntry  dir  -> retD visited' (SymLink stat link dir)
        else do
          let visited' = inode:visited

          if Posix.isDirectory stat
            then retD visited' (DirNode  stat (asDirPath  path))
            else retF visited' (FileNode stat (asFilePath path))

    retF visited' n = return (visited', FileEntry n)
    retD visited' n = return (visited', DirEntry  n)


ls :: (MonadIO m, MonadCatch m) => Bool -> Path Abs File -> IO (LsL m)
ls followSymLinks = go []
  where
    go visited root = do
        (visited', entry) <- discriminatePath followSymLinks visited root

        return $
          case entry of
            FileEntry file ->
                FileP file

            DirEntry dir ->
                DirP dir $ do
                  contents <- liftIO $ getDirectoryContents (toString root)

                  each (sort $ filter valid contents)
                    >-> P.map  (unsafeRelFile . fromJust . dropTrailingSlash . T.pack)
                    >-> P.map  (asDirPath root </>)
                    >-> P.mapM (liftIO . handling . go visited')

    valid :: FilePath -> Bool
    valid ""   = False
    valid "."  = False
    valid ".." = False
    valid _    = True

    handling :: MonadCatch n => n (LsL m) -> n (LsL m)
    handling = handle (\(FSCycleError l)      -> return (FileP (FSCycle l)))
             . handle (\(FileNotFoundError p) -> return (FileP (Missing p)))
