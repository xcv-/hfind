{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module System.Posix.Find where

import Data.Function ((&))

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Data.List (sort)

import Pipes
import qualified Pipes.Prelude as P

import System.Directory
import qualified System.FilePath as FP

import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import System.Posix.Text.Path
import System.Posix.Find.Types
import System.Posix.Find.Combinators
import System.Posix.Find.Ls


-- examples simulating plain unix find

find :: Path Abs Dir -> IO ()
find p = runEffect $
    (ls SymLinksAreFiles p >>= yield)
      >-> onError report
      >-> is resolved
      >-> flatten
      >-> asPaths
      >-> asFiles
      >-> plainText
      >-> P.map T.unpack
      >-> P.stdoutLn

findL :: Path Abs Dir -> IO ()
findL p = runEffect $
    (ls FollowSymLinks p >>= yield)
      >-> followLinks
      >-> onError report
      >-> is resolved
      >-> flatten
      >-> asPaths
      >-> asFiles
      >-> plainText
      >-> P.map T.unpack
      >-> P.stdoutLn
