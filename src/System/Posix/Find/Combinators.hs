{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module System.Posix.Find.Combinators where

import Prelude hiding (filter)

import Data.Bifunctor

import qualified Data.Text as T

import Pipes
import qualified Pipes.Prelude as P

import System.Posix.Text.Path
import System.Posix.Find.Types

import System.IO (hPutStrLn, stderr)


bimapM :: Monad m
       => (fp -> m fp')
       -> (dp -> m dp')
       -> Ls m fp  dp
       -> m (Ls m fp' dp')
bimapM mffp _    (FileP fp)    = FileP <$> mffp fp
bimapM mffp mfdp (DirP dp mbs) = do
    dp' <- mfdp dp
    return $ DirP dp' (mbs >-> P.mapM (bimapM mffp mfdp))


type Transform fp dp m = Pipe (Ls m fp dp) (Ls m fp dp) m ()
type TransformP  m     = Pipe (LsP m)      (LsP m)      m ()
type TransformN  m s   = Pipe (LsN  m s)   (LsN  m s)   m ()
type TransformL  m     = Pipe (LsL  m)     (LsL  m)     m ()
type TransformN' m     = Pipe (LsN' m)     (LsN' m)     m ()
type TransformR  m     = Pipe (LsR  m)     (LsR  m)     m ()


mapChildren :: Monad m
            => (Producer' (Ls m fp dp) m () -> Producer' (Ls m fp dp) m ())
            -> Transform fp dp m
mapChildren f = P.map $ \case
                  DirP dp mbs -> DirP dp (f mbs)
                  file        -> file


fixP :: forall m a b. Monad m
        => ((Producer' a m () -> Producer' b m ()) ->  Pipe a b m ())
        -> Pipe a b m ()
fixP f = f_go
  where
    go :: Producer' a m () -> Producer' b m ()
    go as = as >-> f_go

    f_go :: Pipe a b m ()
    f_go = f go


recurse :: Monad m
         => Pipe (Ls m fp dp) (Ls m fp dp) m ()
         -> Pipe (Ls m fp dp) (Ls m fp dp) m ()
recurse f = fixP (\go -> f >-> mapChildren go)


censor :: Monad m => (Ls m fp dp -> Bool) -> Transform fp dp m
censor p = recurse (P.filter p)

censorM :: Monad m => (Ls m fp dp -> m Bool) -> Transform fp dp m
censorM p = recurse (P.filterM p)


censorF2 :: Monad m => (fp -> Bool) -> (dp -> Bool) -> Transform fp dp m
censorF2 p q = censor p'
  where
    p' (FileP fp)  = p fp
    p' (DirP dp _) = q dp

censorF2M :: Monad m => (fp -> m Bool) -> (dp -> m Bool) -> Transform fp dp m
censorF2M p q = censorM p'
  where
    p' (FileP fp)  = p fp
    p' (DirP dp _) = q dp


censorP :: Monad m => (forall t. IsPathType t => Path Abs t -> Bool) -> TransformP m
censorP p = censorF2 p p

censorPM :: Monad m => (forall t. IsPathType t => Path Abs t -> m Bool) -> TransformP m
censorPM p = censorF2M p p

censorN :: Monad m => (forall t. IsPathType t => FSNode t s -> Bool) -> TransformN m s
censorN p = censorF2 p p

censorNM :: Monad m => (forall t. IsPathType t => FSNode t s -> m Bool) -> TransformN m s
censorNM p = censorF2M p p


skip :: Monad m => (Ls m fp dp -> Bool) -> Transform fp dp m
skip p = censor (not . p)

skipM :: Monad m => (Ls m fp dp -> m Bool) -> Transform fp dp m
skipM p = censorM (fmap not . p)

skipF2 :: Monad m => (fp -> Bool) -> (dp -> Bool) -> Transform fp dp m
skipF2 p q = censorF2 (not . p) (not . q)

skipF2M :: Monad m => (fp -> m Bool) -> (dp -> m Bool) -> Transform fp dp m
skipF2M p q = censorF2M (fmap not . p) (fmap not . q)

skipP :: Monad m => (forall t. IsPathType t => Path Abs t -> Bool) -> TransformP m
skipP p = skipF2 p p

skipPM :: Monad m => (forall t. IsPathType t => Path Abs t -> m Bool) -> TransformP m
skipPM p = skipF2M p p

skipN :: Monad m => (forall t. IsPathType t => FSNode t s -> Bool) -> TransformN m s
skipN p = skipF2 p p

skipNM :: Monad m => (forall t. IsPathType t => FSNode t s -> m Bool) -> TransformN m s
skipNM p = skipF2M p p


pruneDirs :: Monad m => (dp -> Bool) -> Transform fp dp m
pruneDirs = skipF2 (const False)

pruneDirsM :: Monad m => (dp -> m Bool) -> Transform fp dp m
pruneDirsM = skipF2M (const $ return False)


follow :: forall s s' t. (HasErrors s ~ HasErrors s') => FSNode t s -> FSNode t s'
follow (FileNode stat p) = FileNode stat p
follow (DirNode  stat p) = DirNode  stat p
follow (Symlink _ l n)   =
    case follow n :: FSNode t s' of
        FileNode stat _ -> FileNode stat (asFilePath (getLinkPath l))
        DirNode stat _  -> DirNode stat  (asDirPath  (getLinkPath l))
        Missing p       -> Missing p
        FSCycle p       -> FSCycle p
        _ -> error "System.Posix.Find.Combinators.follow: the impossible happened!"
follow (Missing p)       = Missing p
follow (FSCycle l)       = FSCycle l

followLinks :: Monad m => Pipe (LsL m) (LsN' m) m ()
followLinks = P.map (bimap follow follow)


type NodeFilter m s s' = forall t. FSNode t s -> Producer' (FSNode t s') m ()

report :: (MonadIO m, HasLinks s ~ HasLinks s') => NodeFilter m s s'
report = \case
    FileNode stat p  -> yield (FileNode stat p)
    DirNode stat p   -> yield (DirNode stat p)
    Symlink stat l p -> for (report p) (yield . Symlink stat l)
    Missing p        -> liftIO $ hPutStrLn stderr ("*** File not found " ++ show p)
    FSCycle l        -> liftIO $ hPutStrLn stderr ("*** File system cycle at " ++ show l)

silence :: (Monad m, HasLinks s ~ HasLinks s') => NodeFilter m s s'
silence = \case
    FileNode stat p  -> yield (FileNode stat p)
    DirNode stat p   -> yield (DirNode stat p)
    Symlink stat l p -> for (silence p) (yield . Symlink stat l)
    Missing _        -> return ()
    FSCycle _        -> return ()

onError :: (Monad m, HasLinks s ~ HasLinks s')
        => NodeFilter m s s' -> Pipe (LsN m s) (LsN m s') m ()
onError erase = fixP $ \go ->
    for cat $ \case
      FileP fp    -> erase fp >-> P.map FileP
      DirP dp mbs -> erase dp >-> P.map (\dp' -> DirP dp' (go mbs))


clean :: MonadIO m => Pipe (LsL m) (LsR m) m ()
clean = followLinks >-> onError report


flatten :: Monad m => Pipe (Ls m fp dp) (ListEntry fp dp) m ()
flatten =
    for cat $ \case
      FileP fp    -> yield (FileEntry fp)
      DirP dp mbs -> yield (DirEntry  dp) >> (mbs >-> flatten)


asPaths :: Monad m => Pipe (NodeListEntry 'Resolved) PathListEntry m ()
asPaths = P.map (bimap nodePath nodePath)

asFiles :: Monad m => Pipe PathListEntry (Path Abs File) m ()
asFiles = P.map (\case DirEntry  p -> asFilePath p
                       FileEntry p -> asFilePath p)

plainText :: Monad m => Pipe (Path Abs File) T.Text m ()
plainText = P.map toText

stringPaths :: Monad m => Pipe (Path Abs File) String m ()
stringPaths = P.map toString


-- disambiguation

is :: Pipe a a m () -> Pipe a a m ()
is = id

raw :: Monad m => Pipe (LsN m 'Raw) (LsN m 'Raw) m ()
raw = cat

withLinks :: Monad m => Pipe (LsN m 'WithLinks) (LsN m 'WithLinks) m ()
withLinks = cat

withoutLinks :: Monad m => Pipe (LsN m 'WithoutLinks) (LsN m 'WithoutLinks) m ()
withoutLinks = cat

resolved :: Monad m => Pipe (LsN m 'Resolved) (LsN m 'Resolved) m ()
resolved = cat
