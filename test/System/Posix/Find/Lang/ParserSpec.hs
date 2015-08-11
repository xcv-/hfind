{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Posix.Find.Lang.ParserSpec where

import GHC.Exts (IsString(..))

import Data.Either
import Data.Monoid

import qualified Data.Text.ICU.Regex as Regex

import Text.RawString.QQ

import SpecHelper

instance IsString a => IsString (Either a b) where
    fromString = Left . fromString

instance IsString Expr where
    fromString = LitE . StringL . fromString

instance Eq Lit where
    BoolL a    == BoolL b      = a == b
    NumL a     == NumL b       = a == b
    StringL a  == StringL b    = a == b
    RegexL r c == RegexL r' c' = Regex.pattern r == Regex.pattern r' && c == c'
    _          == _            = False

instance Eq Expr where
    LitE a    == LitE b    = a == b
    VarE a    == VarE b    = a == b
    InterpE a == InterpE b = a == b
    _         == _         = False

instance Eq Pred where
    NotP a    == NotP b     = a == b
    AndP a b  == AndP c d   = a == c && b == d
    OrP a b   == OrP c d    = a == c && b == d
    ExprP a   == ExprP b    = a == b
    OpP o a b == OpP o' c d = o == o' && a == c && b == d
    _         == _          = False

spec = do
    let parseE = parseExpr "<literal>"
        parseP = parsePred "<literal>"

    let true   = LitE (BoolL True)
        false  = LitE (BoolL False)
        num i  = LitE (NumL  i)
        var n  = VarE (NamedVar n)
        rxc i  = VarE (RxCapVar i)

    let trueP  = ExprP true
        falseP = ExprP false

    let ivar n = Right (NamedVar n)
        irxc i = Right (RxCapVar i)

        rxIs pat c = \case Right (LitE (RegexL rx cap))
                             | Regex.pattern rx == pat && c == cap -> True
                           _ -> False

    let shouldSatisfyF m p = (p <$> m) `shouldReturn` True

    describe "System.Posix.Find.Lang.Parser" $ do
        context "exprParser" $ do
            it "parses variables" $ do
                parseE [r|$var123|] `shouldReturn`
                    Right (var "var123")

                parseE [r|${var123}|] `shouldReturn`
                    Right (var "var123")

                parseE [r|$123|] `shouldReturn`
                    Right (rxc 122)

                parseE [r|${123}|] `shouldReturn`
                    Right (rxc 122)

                parseE [r|$123var|]   `shouldSatisfyF` isLeft
                parseE [r|${123}var|] `shouldSatisfyF` isLeft

            it "parses literals" $ do
                parseE [r|true|]  `shouldReturn` Right true
                parseE [r|false|] `shouldReturn` Right false
                parseE [r|10|]    `shouldReturn` Right (num 10)

                parseE [r|"asdf\"q$$wer"|] `shouldReturn`
                    Right "asdf\"q$wer"

                let pat   = [r|^@(?:pat\w+\*?)*/\\[^!*\_\S]$|]
                    pat'  = [r|^@(?:pat\w+\*?)*/\\[^!*_\S]$|]

                rx <- Regex.regex [ Regex.CaseInsensitive, Regex.DotAll
                                  , Regex.Multiline, Regex.Comments ]
                                  pat'

                parseE ("m_" <> pat <> "_mxisn") `shouldReturn`
                    Right (LitE (RegexL rx NoCapture))

            it "parses interpolations" $ do
                parseE [r|"$2"|] `shouldReturn`
                    Right (InterpE [irxc 1])

                parseE [r|"$var"|] `shouldReturn`
                    Right (InterpE [ivar "var"])

                parseE [r|"test$1in\"terp"|] `shouldReturn`
                    Right (InterpE ["test", irxc 0, "in\"terp"])

                parseE [r|"test${q}interp"|] `shouldReturn`
                    Right (InterpE ["test", ivar "q", "interp"])

                parseE [r|"${var}test$$$3"|] `shouldReturn`
                    Right (InterpE [ivar "var", "test$", irxc 2])

        context "parsePred" $ do
            it "parses parenthesized predicates" $ do
                parseP [r|( ( (   (false) ) ))|] `shouldReturn`
                    Right (ExprP false)

            it "parses any expression" $ do
                parseP [r|true|] `shouldReturn` Right (ExprP true)
                parseP [r|10|]   `shouldReturn` Right (ExprP (num 10))
                parseP [r|$1|]   `shouldReturn` Right (ExprP (rxc 0))

                parseP [r|"asdf$q"|] `shouldReturn`
                    Right (ExprP (InterpE ["asdf", ivar "q"]))

            it "parses negations" $ do
                parseP [r|not(true)|] `shouldReturn`
                    Right (NotP (ExprP true))

                parseP [r|not(((not(true))))|] `shouldReturn`
                    Right (NotP (NotP (ExprP true)))

                parseP [r|not(57)|] `shouldReturn`
                    Right (NotP (ExprP (num 57)))

            it "parses operator application" $ do
                parseP [r| 1 == 2 |] `shouldReturn`
                    Right (OpP OpEQ (num 1) (num 2))

                parseP [r| $1 == $x |] `shouldReturn`
                    Right (OpP OpEQ (rxc 0) (var "x"))

                parseP [r| $size <= 1000 |] `shouldReturn`
                    Right (OpP OpLE (var "size") (num 1000))

                parseP [r| $size < 1000 |] `shouldReturn`
                    Right (OpP OpLT (var "size") (num 1000))

                parseP [r| $size >= 1000 |] `shouldReturn`
                    Right (OpP OpGE (var "size") (num 1000))

                parseP [r| $size > 1000 |] `shouldReturn`
                    Right (OpP OpGT (var "size") (num 1000))

                let pat = [r|\w\W\Sł€\S\s$|]

                rx <- Regex.regex [Regex.CaseInsensitive, Regex.Comments] pat

                parseP (" $name =~ m#" <> pat <> "#ix ") `shouldReturn`
                    Right (OpP OpRX (var "name") (LitE (RegexL rx Capture)))

            it "parses and/or left-associatively" $ do
                parseP [r| true && false || true |] `shouldReturn`
                    Right ((trueP `AndP` falseP) `OrP` trueP)

                parseP [r| true && not(false || true) && true |] `shouldReturn`
                    Right ((trueP `AndP` NotP (falseP `OrP` trueP)) `AndP` trueP)
