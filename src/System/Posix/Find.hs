{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module System.Posix.Find (module X) where

import Pipes as X
import qualified Pipes.Prelude as P

import System.Posix.Text.Path as X
import System.Posix.Find.Types as X
import System.Posix.Find.Combinators as X
import System.Posix.Find.Ls as X

import System.Posix.Find.Lang.Eval      as X
import System.Posix.Find.Lang.Types     as X
import System.Posix.Find.Lang.Parser    as X
import System.Posix.Find.Lang.Predicate as X


-- examples simulating plain unix find

find :: Path Abs Dir -> IO ()
find p = runEffect $
    (ls symlinksAreFiles p >>= yield)
      >-> onError report
      >-> is resolved
      >-> flatten
      >-> asPaths
      >-> asFiles
      >-> stringPaths
      >-> P.stdoutLn

findL :: Path Abs Dir -> IO ()
findL p = runEffect $
    (ls followSymlinks p >>= yield)
      >-> followLinks
      >-> onError report
      >-> is resolved
      >-> flatten
      >-> asPaths
      >-> asFiles
      >-> stringPaths
      >-> P.stdoutLn
