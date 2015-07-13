module SpecHelper
    ( module Test.Hspec
    , module System.Posix.Text.Path
    , shouldError
    ) where

import Test.Hspec
import System.Posix.Text.Path

import Control.DeepSeq

import Control.Exception as E

shouldError :: NFData a => a -> () -> Expectation
shouldError a () = (a `deepseq` return a) `shouldThrow` sel
  where
    sel :: Selector E.ErrorCall
    sel = const True
