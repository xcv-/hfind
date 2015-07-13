{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module System.Posix.Find.Ls where

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


data FollowSymLinks   = FollowSymLinks
data SymLinksAreFiles = SymLinksAreFiles


class SymLinkStrategy strat where
    type StratResult strat :: FSNodeType

    discriminateLink :: strat
                     -> [Posix.FileID]
                     -> Posix.FileStatus
                     -> (B.ByteString, T.Text, Path Abs File)
                     -> IO (Maybe ([Posix.FileID], NodeListEntry (StratResult strat)))


instance SymLinkStrategy FollowSymLinks where
    type StratResult FollowSymLinks = 'WithLinks

    discriminateLink _ visited stat (bpath, tpath, path)
      | Posix.isSymbolicLink stat = do
          let inode = Posix.fileID stat

          (visited', target) <- do
              when (inode `elem` visited) $ do
                  throwM (FSCycleError tpath)

              tdest <- T.decodeUtf8 <$> Posix.readSymbolicLink bpath

              case canonicalizeBeside path tdest of
                  Right p' -> discriminatePath FollowSymLinks (inode:visited) p'
                  Left _   -> throwM (FileNotFoundError tdest)

          let link = Link path

          return . Just $
            case target of
                FileEntry file -> (visited', FileEntry (SymLink stat link file))
                DirEntry  dir  -> (visited', DirEntry  (SymLink stat link dir))

      | otherwise = return Nothing


instance SymLinkStrategy SymLinksAreFiles where
    type StratResult SymLinksAreFiles = 'WithoutLinks

    discriminateLink _ _ _ _ = return Nothing


discriminatePath :: SymLinkStrategy strat
                 => strat
                 -> [Posix.FileID]
                 -> Path Abs File
                 -> IO ([Posix.FileID], NodeListEntry (StratResult strat))
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


ls :: forall m io strat.
        ( MonadIO m, MonadIO io, MonadCatch m
        , SymLinkStrategy strat, HasErrors (StratResult strat) ~ 'True )
   => strat
   -> Path Abs Dir
   -> io (LsN m (StratResult strat))
ls strat = liftIO . go []
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

    handling :: IO (LsN m (StratResult strat)) -> IO (LsN m (StratResult strat))
    handling = handle (\(FSCycleError l)      -> return (FileP (FSCycle l)))
             . handle (\(FileNotFoundError p) -> return (FileP (Missing p)))
