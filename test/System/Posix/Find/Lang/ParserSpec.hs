{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Posix.Find.Lang.ParserSpec where

import GHC.Exts (IsString(..))

import Data.Either
import Data.Monoid

import qualified Data.Text.ICU as ICU

import Text.RawString.QQ (r)

import SpecHelper

instance IsString a => IsString (Either a b) where
    fromString = Left . fromString

instance IsString Expr where
    fromString = LitE . StringL . fromString

instance Eq Lit where
    BoolL a    == BoolL b      = a == b
    NumL a     == NumL b       = a == b
    StringL a  == StringL b    = a == b
    _          == _            = False

instance Eq Expr where
    LitE a    == LitE b    = a == b
    VarE a    == VarE b    = a == b
    InterpE a == InterpE b = a == b
    AppE f x  == AppE g y  = f == g && x == y
    _         == _         = False

instance Eq Pred where
    NotP a       == NotP b          = a == b
    AndP a b     == AndP c d        = a == c && b == d
    OrP a b      == OrP c d         = a == c && b == d
    ExprP a      == ExprP b         = a == b
    OpP o a b    == OpP o' c d      = o == o' && a == c && b == d
    MatchP s r c == MatchP s' r' c' = s == s' && c == c'
                                     && ICU.pattern r == ICU.pattern r'
    _         == _          = False

spec = do
    let parseE = parseExpr "<literal>"
        parseP = parsePred "<literal>"

    let true   = LitE (BoolL True)
        false  = LitE (BoolL False)
        num i  = LitE (NumL  i)
        str s  = LitE (StringL s)
        var n  = VarE (NamedVar n)
        rxc i  = VarE (RxCapVar i)

    let trueP  = ExprP true
        falseP = ExprP false

    let ivar n = Right (NamedVar n)
        irxc i = Right (RxCapVar i)

    describe "System.Posix.Find.Lang.Parser" $ do
        context "exprParser" $ do
            it "parses variables" $ do
                parseE [r|$var123|] `shouldBe`
                    Right (var "var123")

                parseE [r|${var123}|] `shouldBe`
                    Right (var "var123")

                parseE [r|$123|] `shouldBe`
                    Right (rxc 123)

                parseE [r|${123}|] `shouldBe`
                    Right (rxc 123)

                parseE [r|$123var|]   `shouldSatisfy` isLeft
                parseE [r|${123}var|] `shouldSatisfy` isLeft

            it "parses literals" $ do
                parseE [r|true|]  `shouldBe` Right true
                parseE [r|false|] `shouldBe` Right false
                parseE [r|10|]    `shouldBe` Right (num 10)

                parseE [r|"asdf\"q$$wer"|] `shouldBe`
                    Right "asdf\"q$wer"

            it "parses interpolations" $ do
                parseE [r|"$2"|] `shouldBe`
                    Right (InterpE [irxc 2])

                parseE [r|"$var"|] `shouldBe`
                    Right (InterpE [ivar "var"])

                parseE [r|"test$1in\"terp"|] `shouldBe`
                    Right (InterpE ["test", irxc 1, "in\"terp"])

                parseE [r|"test${q}interp"|] `shouldBe`
                    Right (InterpE ["test", ivar "q", "interp"])

                parseE [r|"${var}test$$$3"|] `shouldBe`
                    Right (InterpE [ivar "var", "test$", irxc 3])

            it "parses function applications" $ do
                parseE [r|f g $v|] `shouldBe`
                    Right (AppE "f" (AppE "g" (var "v")))

                parseE [r|(f g) (g i)|] `shouldSatisfy` isLeft


        context "parsePred" $ do
            it "parses parenthesized predicates" $ do
                parseP [r|( ( (   (false) ) ))|] `shouldBe`
                    Right (ExprP false)

            it "parses any expression" $ do
                parseP [r|true|] `shouldBe` Right (ExprP true)
                parseP [r|10|]   `shouldBe` Right (ExprP (num 10))
                parseP [r|$1|]   `shouldBe` Right (ExprP (rxc 1))

                parseP [r|"asdf$q"|] `shouldBe`
                    Right (ExprP (InterpE ["asdf", ivar "q"]))

            it "parses negations" $ do
                parseP [r|not(true)|] `shouldBe`
                    Right (NotP (ExprP true))

                parseP [r|not(((not(true))))|] `shouldBe`
                    Right (NotP (NotP (ExprP true)))

                parseP [r|not(57)|] `shouldBe`
                    Right (NotP (ExprP (num 57)))

            it "parses operator application" $ do
                parseP [r| 1 == 2 |] `shouldBe`
                    Right (OpP OpEQ (num 1) (num 2))

                parseP [r| $1 == $x |] `shouldBe`
                    Right (OpP OpEQ (rxc 1) (var "x"))

                parseP [r| $size <= 1000 |] `shouldBe`
                    Right (OpP OpLE (var "size") (num 1000))

                parseP [r| $size < 1000 |] `shouldBe`
                    Right (OpP OpLT (var "size") (num 1000))

                parseP [r| $size >= 1000 |] `shouldBe`
                    Right (OpP OpGE (var "size") (num 1000))

                parseP [r| $size > 1000 |] `shouldBe`
                    Right (OpP OpGT (var "size") (num 1000))

            it "parses complex regexes with escape sequences" $ do
                let pat  = [r|^@(?:pat\w+\*?)*/\\[^!*\_\S]$|]
                    pat' = [r|^@(?:pat\w+\*?)*/\\[^!*_\S]$|]

                let rx = ICU.regex [ ICU.CaseInsensitive, ICU.DotAll
                                   , ICU.Multiline, ICU.Comments ]
                                   pat'

                parseP ("\"\" =~ m_" <> pat <> "_mxisn") `shouldBe`
                    Right (MatchP (str "") rx NoCapture)

            it "parses unicode regexes" $ do
                let pat = [r|\w\W\Sł€\S\s$|]

                let rx = ICU.regex [ICU.CaseInsensitive, ICU.Comments] pat

                parseP (" $name =~ m#" <> pat <> "#ix ") `shouldBe`
                    Right (MatchP (var "name") rx Capture)

            it "parses and/or left-associatively" $ do
                parseP [r| true && false || true |] `shouldBe`
                    Right ((trueP `AndP` falseP) `OrP` trueP)

                parseP [r| true && not(false || true) && true |] `shouldBe`
                    Right ((trueP `AndP` NotP (falseP `OrP` trueP)) `AndP` trueP)
