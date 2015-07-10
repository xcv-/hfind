{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module System.Posix.Find.Ls where

import Data.Function ((&))

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch

import Pipes
import qualified Pipes.Prelude as P

import Path

import System.Directory
import qualified System.FilePath as FP

import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import System.Posix.Find.Types
import System.Posix.Find.Combinators


type NodeListEntry s = ListEntry (FSNode File s) (FSNode Dir s)


readSymlink :: Link -> IO (Path Abs File)
readSymlink (Link p) = do
    dest <- Posix.readSymbolicLink (toFilePath p)

    dest & FP.dropTrailingPathSeparator
         & toAbsolute
         & canonicalize
         & liftA2 handleAll (const . throwM . FileNotFoundError) parseAbsFile
  where
    toAbsolute p'
      | FP.isAbsolute p' = p'
      | otherwise        = toFilePath (parent p) FP.</> p'

    canonicalize = FP.joinPath . snd . shortcircuit (0, []) . FP.splitDirectories . FP.normalise

    shortcircuit :: (Int, [String]) -> [String] -> (Int, [String])
    shortcircuit = foldr $ \case ".." -> \(i, acc) -> (i+1, acc)
                                 d    -> \(i, acc) -> if i == 0 then (0, d:acc) else (i-1, acc)


discriminatePath :: [Posix.FileID] -> Path Abs File -> IO (Posix.FileID, NodeListEntry 'WithLinks)
discriminatePath = go
  where
    go visited path = do
      let fpath = toFilePath path

      stat <- Posix.getSymbolicLinkStatus fpath
                `catchAll` \_ -> throwM (FileNotFoundError fpath)

      let inode = Posix.fileID stat

      if Posix.isSymbolicLink stat
        then do
          let link = Link path

          (inode', target) <- do
              when (inode `elem` visited) $ do
                  throwM (FSCycleError link)

              go (inode:visited) =<< readSymlink link

          case target of
              DirEntry  dir  -> return (inode', DirEntry  (SymLink stat link dir))
              FileEntry file -> return (inode', FileEntry (SymLink stat link file))
        else do
          if Posix.isDirectory stat
            then return (inode, DirEntry  (DirNode  stat (asDirPath  path)))
            else return (inode, FileEntry (FileNode stat (asFilePath path)))


lsR :: (MonadIO m, MonadCatch m) => Path Abs File -> IO (LsL m)
lsR = go []
  where
    go visited root = do
        discriminatePath visited root <&> \case
            (_, FileEntry file) ->
                FileP file

            (inode, DirEntry dir) ->
                DirP dir $ do
                  contents <- liftIO $ getDirectoryContents (toFilePath root)

                  each (filter valid contents)
                    >-> P.mapM (parseRelFile . FP.dropTrailingPathSeparator)
                    >-> P.map  (asDirPath root </>)
                    >-> P.mapM (liftIO . handling . go (inode:visited))

    valid :: FilePath -> Bool
    valid ".." = False
    valid "."  = False
    valid _    = True

    handling :: MonadCatch n => n (LsL m) -> n (LsL m)
    handling = handle (\(FSCycleError l)      -> return (FileP (FSCycle l)))
             . handle (\(FileNotFoundError p) -> return (FileP (Missing p)))
