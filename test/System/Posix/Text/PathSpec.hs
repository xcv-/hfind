{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Posix.Text.PathSpec where

import Data.Either

import SpecHelper

spec :: Spec
spec = do
    describe "System.Posix.Text.Path" $ do
        let rdir  = unsafeRelDir
            rfile = unsafeRelFile
            adir  = unsafeAbsDir
            afile = unsafeAbsFile

        context "asDirPath" $ do
            let coerceFD = toText . asDirPath . afile
                coerceDD = toText . asDirPath . adir

            it "adds a trailing slash to files" $ do
                coerceFD ""     `shouldBe` "/"
                coerceFD "a"    `shouldBe` "a/"
                coerceFD "a/b"  `shouldBe` "a/b/"
                coerceFD "/a/b" `shouldBe` "/a/b/"

            it "does nothing to directories" $ do
                coerceDD "/"     `shouldBe` "/"
                coerceDD "a/"    `shouldBe` "a/"
                coerceDD "a/b/"  `shouldBe` "a/b/"
                coerceDD "/a/b/" `shouldBe` "/a/b/"

        context "asFilePath" $ do
            let coerceFF = toText . asFilePath . afile
                coerceDF = toText . asFilePath . adir

            it "does nothing to files" $ do
                coerceFF ""     `shouldBe` ""
                coerceFF "a"    `shouldBe` "a"
                coerceFF "a/b"  `shouldBe` "a/b"
                coerceFF "/a/b" `shouldBe` "/a/b"

            it "removes the trailing slash to directories" $ do
                coerceDF "/"     `shouldBe` ""
                coerceDF "a/"    `shouldBe` "a"
                coerceDF "a/b/"  `shouldBe` "a/b"
                coerceDF "/a/b/" `shouldBe` "/a/b"

        context "</>" $ do
            it "appends directories correctly" $ do
                adir "/" </> rdir "a/"   `shouldBe` adir "/a/"
                adir "/" </> rdir "a/b/" `shouldBe` adir "/a/b/"

            it "appends files correctly" $ do
                adir "/" </> rfile "a"   `shouldBe` afile "/a"
                adir "/" </> rfile "a/b" `shouldBe` afile "/a/b"

            it "appends relative paths correctly" $ do
                rdir "a/" </> rdir "b/" </> rfile "c"  `shouldBe` rfile "a/b/c"
                rdir "a/" </> rdir "b/" </> rdir  "c/" `shouldBe` rdir  "a/b/c/"


        context "addTrailingSlash" $ do
            it "works" $ do
                addTrailingSlash ""      `shouldBe` Nothing
                addTrailingSlash "a"     `shouldBe` Just "a/"
                addTrailingSlash "/"     `shouldBe` Just "/"
                addTrailingSlash "/a"    `shouldBe` Just "/a/"
                addTrailingSlash "/a/b"  `shouldBe` Just "/a/b/"
                addTrailingSlash "/a/b/" `shouldBe` Just "/a/b/"

        context "addTrailingSlash" $ do
            it "works" $ do
                addTrailingSlash ""      `shouldBe` Nothing
                addTrailingSlash "a"     `shouldBe` Just "a/"
                addTrailingSlash "/"     `shouldBe` Just "/"
                addTrailingSlash "/a"    `shouldBe` Just "/a/"
                addTrailingSlash "/a/b"  `shouldBe` Just "/a/b/"
                addTrailingSlash "/a/b/" `shouldBe` Just "/a/b/"

        context "parent" $ do
            let parentD = parent . adir
                parentF = parent . afile

            it "doesn't go higher than '/'" $ do
                parentF ""       `shouldBe` Nothing
                parentD "/"      `shouldBe` Nothing

            it "works near the root" $ do
                parentD "/a/"         `shouldBe` Just (adir "/")
                parentF "/a"          `shouldBe` Just (adir "/")

            it "works further the root" $ do
                parentD "/a/b/c/d/"   `shouldBe` Just (adir "/a/b/c/")
                parentF "/a/b/c/d"    `shouldBe` Just (adir "/a/b/c/")

        context "normalize" $ do
            it "eats trailing slashes" $ do
                normalize "/"            `shouldBe` Just ""
                normalize "/a/"          `shouldBe` Just "/a"

            it "skips double slashes" $ do
                normalize "////"         `shouldBe` Just ""
                normalize "////a////"    `shouldBe` Just "/a"

            it "navigates correctly" $ do
                normalize "/a/../b/"     `shouldBe` Just "/b"
                normalize "/a/..//b/"    `shouldBe` Just "/b"
                normalize "a/b/../../c"  `shouldBe` Just "c"

            it "skips dots and navigates correctly" $ do
                normalize "a/./../d/c/." `shouldBe` Just "d/c"
                normalize "./././././."  `shouldBe` Just ""

            it "does not go up too much" $ do
                normalize "a/b/../../.." `shouldBe` Nothing
                normalize "/../a"        `shouldBe` Nothing

        context "canonicalizeUnder" $ do
            let shallow = adir "/a/"
                deep    = adir "/a/b/c/d/"

            let file = Right . afile

            it "appends the relative path" $ do
                canonicalizeUnder shallow "x/y"  `shouldBe` file "/a/x/y"

            it "normalizes" $ do
                canonicalizeUnder shallow "x/y/" `shouldBe` file "/a/x/y"
                canonicalizeUnder shallow "./y/" `shouldBe` file "/a/y"
                canonicalizeUnder shallow "../z" `shouldBe` file "/z"

            it "navigates up to the root" $ do
                canonicalizeUnder shallow ".."        `shouldBe` file ""
                canonicalizeUnder deep "../../../.."  `shouldBe` file ""

            it "does not prepend to absolute paths" $ do
                canonicalizeUnder deep "/"         `shouldBe` file ""
                canonicalizeUnder deep "/x/y/z/"   `shouldBe` file "/x/y/z"
                canonicalizeUnder deep "/././z/.." `shouldBe` file ""

        context "canonicalizeBeside" $ do
            let root     = adir "/"
                shallowF = adir "/b"
                shallowD = adir "/b/"
                deepF    = adir "/a/b/c/d"
                deepD    = adir "/a/b/c/d/"

            let file = Right . unsafeAbsFile

            it "works at the root" $ do
                canonicalizeBeside shallowF "b" `shouldBe` file "/b"
                canonicalizeBeside shallowD "b" `shouldBe` file "/b"

            it "doesn't go further than '/'" $ do
                canonicalizeBeside root "a"   `shouldSatisfy` isLeft

            it "works with relative, non-normalized paths" $ do
                canonicalizeBeside deepF "..///././z/../y/" `shouldBe` file "/a/b/y"
                canonicalizeBeside deepD "..///././z/../y"  `shouldBe` file "/a/b/y"


main :: IO ()
main = hspec spec
