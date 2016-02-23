{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module System.HFind.CombinatorsSpec where

import GHC.Exts

import Pipes
import qualified Pipes.Prelude as P

import Data.Bifunctor
import Control.Monad.Identity

import SpecHelper

instance IsString (Path Abs t) where
    fromString = Path . fromString

spec = do
    let file fp   = FileP fp
        dir dp bs = DirP dp (each bs)

    let samplei :: Walk Identity Int Int
        samplei = dir 0 [ file 1
                        , dir 2 [ file 3
                                , file 4
                                , dir 5 [ file 9  ]
                                , dir 7 [ file 11 ]
                                ]
                        , dir 4 [ file 12
                                , file 14
                                ]
                        ]

        samplep :: WalkP Identity
        samplep = dir "/" [ file "/a"
                          , file "/b"
                          , dir "/tmp/" [ file "/tmp/adb.log"
                                        , file "/tmp/a.out"
                                        , file "/tmp/xauth-1000-_0"
                                        , dir  "/tmp/systemd-private/" []
                                        ]
                          , dir "/usr/" [ dir "/usr/share/"   []
                                        , dir "/usr/bin/"     []
                                        , dir "/usr/include/" []
                                        , dir "/usr/lib/"     []
                                        ]
                          ]

        over :: a -> Pipe a b Identity () -> [b]
        over x f = P.toList (yield x >-> f)

    describe "System.Posix.Find.Combinators" $ do
        context "bimap" $ do
            -- free theorems!
            it "Walk m is a bifunctor" $ do
                bimap id id samplei `shouldBe` samplei
                bimap id id samplep `shouldBe` samplep

        context "skip" $ do
            it "skips even files and odd directories" $ do
                over samplei (skipF2 even odd) `shouldBe`
                    [ dir 0 [ file 1
                            , dir 2 [ file 3 ]
                            , dir 4 []
                            ]
                    ]
