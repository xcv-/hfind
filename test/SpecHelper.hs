{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module SpecHelper
    ( module Test.Hspec
    , module Test.Hspec.QuickCheck
    , module System.Posix.Text.Path
    , module System.Posix.Find.Types
    , module System.Posix.Find.Combinators
    , module System.Posix.Find.Lang.Builtins
    , module System.Posix.Find.Lang.Eval
    , module System.Posix.Find.Lang.Parser
    , module System.Posix.Find.Lang.Predicate
    , module System.Posix.Find.Lang.Types
    , shouldError
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Bifunctor
import Data.List
import Data.Monoid

import Control.DeepSeq
import Control.Exception as E
import Control.Monad.Identity

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Pipes
import qualified Pipes.Prelude as P

import System.IO (stderr, hPutStrLn)
import System.IO.Unsafe

import System.Posix.Text.Path
import System.Posix.Find.Types
import System.Posix.Find.Combinators

import System.Posix.Find.Lang.Builtins
import System.Posix.Find.Lang.Eval
import System.Posix.Find.Lang.Parser
import System.Posix.Find.Lang.Predicate
import System.Posix.Find.Lang.Types


shouldError :: NFData a => a -> () -> Expectation
shouldError a () = (a `deepseq` return a) `shouldThrow` sel
  where
    sel :: Selector E.ErrorCall
    sel = const True


newtype FileName = FileName { getFileName :: T.Text }

instance Arbitrary FileName where
    arbitrary = do
        n <- arbitrary
        if T.null n || n == "." || n == ".." || ("/" `T.isInfixOf` n)
          then arbitrary
          else return (FileName n)


instance Arbitrary (Path Abs Dir) where
    arbitrary = do
        Positive (Small depth) <- arbitrary
        pieces <- replicateM depth (getFileName <$> arbitrary)
        return . unsafeAbsDir $ "/" <> T.intercalate "/" pieces <> "/"
      where

    shrink p =
        case parent p of
            Just par -> par : shrink par
            Nothing  -> []

dirToFile :: Path Abs Dir -> Path Abs File
dirToFile = asFilePath

fileToDir :: Path Abs File -> Path Abs Dir
fileToDir = asDirPath


instance Arbitrary (Path Abs File) where
    arbitrary = fmap dirToFile arbitrary
    shrink    = map dirToFile . shrink . fileToDir


instance (m ~ Identity) => Arbitrary (Walk m FileName FileName) where
    arbitrary = do
        file <- arbitrary

        if file
          then do
            fp <- arbitrary
            return (FileP fp)
          else do
            dp <- arbitrary
            --Small numSubdirs <- arbitrary
            let numSubdirs = 0
            subdirs <- replicateM numSubdirs arbitrary
            return (DirP dp (each (subdirs `asTypeOf` [])))

    shrink (FileP fp)    = []
    shrink (DirP dp mbs) =
      [DirP dp (each subseq) | subseq <- subsequences (P.toList mbs)]


instance (m ~ Identity) => Arbitrary (Walk m (Path Abs File) (Path Abs Dir)) where
    arbitrary = do
        relative <- bimap getFileName getFileName <$> arbitrary

        absolute <$> arbitrary <*> pure relative
      where
        absolute :: Path Abs Dir -> Walk Identity T.Text T.Text -> WalkP Identity
        absolute (Path par) (FileP fp) =
            FileP (unsafeAbsFile (par <> "/" <> fp))
        absolute par (DirP dp mbs) =
            DirP par' (mbs >-> P.map (absolute par'))
          where
            par' = unsafeAbsDir $ toText par <> dp <> "/"

    shrink (FileP fp)    = []
    shrink (DirP dp mbs) =
      [DirP dp (each subseq) | subseq <- subsequences (P.toList mbs)]


instance (m ~ Identity, Ord fp, Ord dp) => Eq (Walk m fp dp) where
    FileP fp == FileP fp'        = fp == fp'
    DirP dp mbs == DirP dp' mbs' = dp == dp' &&
                                   sort (P.toList mbs) == sort (P.toList mbs')
    _ == _ = False

instance (m ~ Identity, Ord fp, Ord dp) => Ord (Walk m fp dp) where
    FileP fp  `compare` FileP fp'   = fp `compare` fp'
    DirP dp _ `compare` DirP dp' _  = dp `compare` dp'
    FileP _   `compare` DirP _ _    = LT
    DirP _ _  `compare` FileP _     = GT

instance (m ~ Identity, Show fp, Show dp) => Show (Walk m fp dp) where
    show (FileP fp)    = "FileP " ++ show fp
    show (DirP dp mbs) = "DirP " ++ show dp ++ " " ++ show (P.toList mbs)
