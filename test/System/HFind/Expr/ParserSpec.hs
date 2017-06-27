{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module System.HFind.Expr.ParserSpec where


import Data.Either
import Data.Monoid
import Data.String (IsString(..))
import Data.Text   (Text)

import qualified Data.Text.ICU as ICU

import Text.RawString.QQ (r)

import SpecHelper

instance IsString (Interp a) where
    fromString = InterpLit . fromString

instance IsString Lit where
    fromString = StringL . fromString

instance IsString Expr where
    fromString = LitE . fromString

instance IsString a => IsString (NoLoc a) where
    fromString = NoLoc . fromString

instance Eq Expr where
    LitE a    == LitE b    = a == b
    VarE a    == VarE b    = a == b
    InterpE a == InterpE b = a == b
    AppE f x  == AppE g y  = f == g && x == y
    PlusE a b == PlusE x y = a == x && b == y
    MultE a b == MultE x y = a == x && b == y
    NegE a    == NegE b    = a == b
    _         == _         = False

instance Eq Pred where
    ScopeP a     == ScopeP b        = a == b
    NotP a       == NotP b          = a == b
    AndP a b     == AndP c d        = a == c && b == d
    OrP a b      == OrP c d         = a == c && b == d
    ExprP a      == ExprP b         = a == b
    OpP o a b    == OpP o' c d      = o == o' && a == c && b == d
    MatchP s r c == MatchP s' r' c' = s == s' && c == c'
                                     && ICU.pattern r == ICU.pattern r'
    _         == _          = False

spec = do
    let parseE = parseExpr "<literal>" :: Text -> Either ParseError (NoLoc Expr)
        parseP = parsePred "<literal>" :: Text -> Either ParseError (NoLoc Pred)

    let true    = NoLoc $ LitE (NoLoc $ BoolL True)
        false   = NoLoc $ LitE (NoLoc $ BoolL False)
        num i   = NoLoc $ LitE (NoLoc $ NumL  i)
        str s   = NoLoc $ LitE (NoLoc $ StringL s)
        var n   = NoLoc $ VarE (NoLoc $ NamedVar n)
        rxc i   = NoLoc $ VarE (NoLoc $ RxCapVar i)
        intrp i = NoLoc $ InterpE i
        app h x = NoLoc $ AppE h x

    let trueP  = NoLoc $ ExprP true
        falseP = NoLoc $ ExprP false

    let ivar n = InterpVar (NoLoc $ NamedVar n)
        irxc i = InterpVar (NoLoc $ RxCapVar i)

    let a +: b = NoLoc (PlusE a b)
        a *: b = NoLoc (MultE a b)
        neg a  = NoLoc (NegE a)
        a -: b = NoLoc (PlusE a (neg b))

        a &: b = NoLoc (AndP a b)
        a |: b = NoLoc (OrP a b)
        no a   = NoLoc (NotP a)

    describe "System.HFind.Expr.Parser" $ do
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
                    Right (intrp [irxc 2])

                parseE [r|"$var"|] `shouldBe`
                    Right (intrp [ivar "var"])

                parseE [r|"test$1in\"terp"|] `shouldBe`
                    Right (intrp ["test", irxc 1, "in\"terp"])

                parseE [r|"test${q}interp"|] `shouldBe`
                    Right (intrp ["test", ivar "q", "interp"])

                parseE [r|"${var}test$$$3"|] `shouldBe`
                    Right (intrp [ivar "var", "test$", irxc 3])

            it "parses function applications" $ do
                parseE [r|f g $v|] `shouldBe`
                    Right (app "f" (app "g" (var "v")))

                parseE [r|(f g) (g i)|] `shouldSatisfy` isLeft

            it "parses simple arithmetic expressions" $ do
                parseE [r| 1 + 2 + 3 |] `shouldBe`
                    Right ((num 1 +: num 2) +: num 3)

                parseE [r| 1 * 2 + 3 * 4 - 5|] `shouldBe`
                    Right (((num 1 *: num 2) +: (num 3 *: num 4)) -: num 5)

                parseE [r| 1 * (2 + 3) * 4 - 5|] `shouldBe`
                    Right (((num 1 *: (num 2 +: num 3)) *: num 4) -: num 5)

            it "arithmetic expressions have appropiate precedence" $ do
                parseP [r| 1 + 2 and 3 + 4 |] `shouldBe`
                    Right (NoLoc (ExprP (num 1 +: num 2))
                            &:
                           NoLoc (ExprP (num 3 +: num 4)))

                parseE [r| f g 2 + 1 |] `shouldBe`
                    Right (app "f" (app "g" (num 2)) +: num 1)

                parseE [r| 1 + f g $x * 3 + -1 |] `shouldBe`
                    Right ((num 1 +: (app "f" (app "g" (var "x")) *: num 3)) +: neg (num 1))

                parseE [r| 1 + 2 * f g (1 - 2)|] `shouldBe`
                    Right (num 1 +: (num 2 *: app "f" (app "g" (num 1 -: num 2))))


        context "parsePred" $ do
            it "parses parenthesized predicates" $ do
                parseP [r|( ( (   (false) ) ))|] `shouldBe`
                    Right (NoLoc $ ExprP false)

            it "parses any expression" $ do
                parseP [r|true|] `shouldBe` Right (NoLoc $ ExprP true)
                parseP [r|10|]   `shouldBe` Right (NoLoc $ ExprP $ num 10)
                parseP [r|$1|]   `shouldBe` Right (NoLoc $ ExprP $ rxc 1)

                parseP [r|"asdf$q"|] `shouldBe`
                    Right (NoLoc $ ExprP $ NoLoc $ InterpE ["asdf", ivar "q"])

            it "parses function applications overlapping with keywords" $ do
                parseP [r|stat "asdf"|] `shouldBe`
                    Right (NoLoc $ ExprP $ NoLoc $ AppE "stat" (str "asdf"))

                parseP [r|note 3|] `shouldBe`
                    Right (NoLoc $ ExprP $ NoLoc $ AppE "note" (num 3))

            it "parses explicit scopes" $ do
                parseP [r|scope(true)|] `shouldBe`
                    Right (NoLoc $ ScopeP $ NoLoc $ ExprP true)

            it "parses negations" $ do
                parseP [r|not(true)|] `shouldBe`
                    Right (NoLoc $ NotP $ NoLoc $ ExprP true)

                parseP [r|not(((not(true))))|] `shouldBe`
                    Right (NoLoc $ NotP $ NoLoc $ NotP $ NoLoc $ ExprP true)

                parseP [r|not(57)|] `shouldBe`
                    Right (NoLoc $ NotP $ NoLoc $ ExprP $ num 57)

            it "parses operator application" $ do
                parseP [r| 1 == 2 |] `shouldBe`
                    Right (NoLoc $ OpP OpEQ (num 1) (num 2))

                parseP [r| $1 == $x |] `shouldBe`
                    Right (NoLoc $ OpP OpEQ (rxc 1) (var "x"))

                parseP [r| $size <= 1000 |] `shouldBe`
                    Right (NoLoc $ OpP OpLE (var "size") (num 1000))

                parseP [r| $size < 1000 |] `shouldBe`
                    Right (NoLoc $ OpP OpLT (var "size") (num 1000))

                parseP [r| $size >= 1000 |] `shouldBe`
                    Right (NoLoc $ OpP OpGE (var "size") (num 1000))

                parseP [r| $size > 1000 |] `shouldBe`
                    Right (NoLoc $ OpP OpGT (var "size") (num 1000))

            it "parses complex regexes with escape sequences" $ do
                let pat  = [r|^@(?:pat\w+\*?)*/\\[^!*\_\S]$|]
                    pat' = [r|^@(?:pat\w+\*?)*/\\[^!*_\S]$|]

                let rx = ICU.regex [ ICU.CaseInsensitive, ICU.DotAll
                                   , ICU.Multiline, ICU.Comments ]
                                   pat'

                parseP ("\"\" =~ m_" <> pat <> "_mxisn") `shouldBe`
                    Right (NoLoc $ MatchP (str "") rx NoCapture)

            it "parses unicode regexes" $ do
                let pat = [r|\w\W\Sł€\S\s$|]

                let rx = ICU.regex [ICU.CaseInsensitive, ICU.Comments] pat

                parseP (" $name =~ m#" <> pat <> "#ix ") `shouldBe`
                    Right (NoLoc $ MatchP (var "name") rx Capture)

            it "not takes precedence over and/or" $ do
                parseP [r| not true |] `shouldBe`
                    Right (no trueP)

                parseP [r| not true and false |] `shouldBe`
                    Right (no trueP &: falseP)

                parseP [r| not true and not false or true|] `shouldBe`
                    Right (no trueP &: no falseP |: trueP)

            it "parses and/or left-associatively" $ do
                parseP [r| true and false or true |] `shouldBe`
                    Right ((trueP &: falseP) |: trueP)

                parseP [r| true and not(false or true) and true |] `shouldBe`
                    Right ((trueP &: no (falseP |: trueP)) &: trueP)

                parseP [r| true and (true or true and true) or true |] `shouldBe`
                    Right ((trueP &: ((trueP |: trueP) &: trueP)) |: trueP)
