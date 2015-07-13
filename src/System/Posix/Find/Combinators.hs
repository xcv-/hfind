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

import Pipes
import qualified Pipes.Prelude as P

import System.Posix.Text.Path
import System.Posix.Find.Types

import System.IO (hPutStrLn, stderr)


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)


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
type TransformN  m s   = Pipe (LsN  m s)   (LsN  m s)   m ()
type TransformL  m s   = Pipe (LsL  m)     (LsL  m)     m ()
type TransformN' m s   = Pipe (LsN' m)     (LsN' m)     m ()
type TransformR  m s   = Pipe (LsR  m)     (LsR  m)     m ()


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
censor p = censorM (return . p)

censorM :: Monad m => (Ls m fp dp -> m Bool) -> Transform fp dp m
censorM p = recurse (P.filterM p)


skip :: Monad m => (Ls m fp dp -> Bool) -> Transform fp dp m
skip p = skipM (return . p)

skipM :: Monad m => (Ls m fp dp -> m Bool) -> Transform fp dp m
skipM p = censorM (fmap not . p)


censorP :: Monad m => (Either fp dp -> Bool) -> Transform fp dp m
censorP p = censorPM (return . p)

censorPM :: Monad m => (Either fp dp -> m Bool) -> Transform fp dp m
censorPM p = censorM p'
  where
    p' (FileP fp)  = p (Left fp)
    p' (DirP dp _) = p (Right dp)


censorN :: Monad m => (forall t. FSNode t s -> Bool) -> TransformN m s
censorN p = censorNM (return . p)

censorNM :: Monad m => (forall t. FSNode t s -> m Bool) -> TransformN m s
censorNM p = censorM p'
  where
    p' (FileP fp)  = p fp
    p' (DirP dp _) = p dp


pruneDirs :: Monad m => (dp -> Bool) -> Transform fp dp m
pruneDirs p = pruneDirsM (return . p)

pruneDirsM :: Monad m => (dp -> m Bool) -> Transform fp dp m
pruneDirsM p = skipM p'
  where
    p' (FileP _)   = return False
    p' (DirP dp _) = p dp


follow :: forall s s' t. (HasErrors s ~ HasErrors s') => FSNode t s -> FSNode t s'
follow (FileNode stat p) = FileNode stat p
follow (DirNode  stat p) = DirNode  stat p
follow (SymLink _ l n)   =
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
    SymLink stat l p -> for (report p) (yield . SymLink stat l)
    Missing p        -> liftIO $ hPutStrLn stderr ("*** File not found " ++ show p)
    FSCycle l        -> liftIO $ hPutStrLn stderr ("*** File system cycle at " ++ show l)

silence :: (Monad m, HasLinks s ~ HasLinks s') => NodeFilter m s s'
silence = \case
    FileNode stat p  -> yield (FileNode stat p)
    DirNode stat p   -> yield (DirNode stat p)
    SymLink stat l p -> for (silence p) (yield . SymLink stat l)
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
