import Data.Fix
import qualified Data.Text as T

import Lib

import Nix.Atoms
import Nix.Expr

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "parseStr" $ do
        it "parses correctly an example file" $ do
            parseStr "{a=true;b=[1 2 3];c=null;}" `shouldBe`
                Fix (NSet [
                    NamedVar [StaticKey (T.pack "a")]
                        (Fix (NConstant (NBool True))),
                    NamedVar [StaticKey (T.pack "b")]
                        (Fix (NList [
                            Fix (NConstant (NInt 1)),
                            Fix (NConstant (NInt 2)),
                            Fix (NConstant (NInt 3))
                        ])),
                    NamedVar [StaticKey (T.pack "c")]
                        (Fix (NConstant NNull))
                ])

    describe "exprIndent" $ do
        it "indents constants" $ do
            exprIndent (parseStr "42") `shouldBe` "42"
            exprIndent (parseStr "true") `shouldBe` "true"
            exprIndent (parseStr "false") `shouldBe` "false"
            exprIndent (parseStr "null") `shouldBe` "null"
            exprIndent (parseStr "https://example.org") `shouldBe`
                "https://example.org"
        it "indents strings" $ do
            exprIndent (parseStr "\"foo bar\"") `shouldBe` "\"foo bar\""
            exprIndent (parseStr "''foo bar''") `shouldBe` "\"foo bar\""
            exprIndent (parseStr "''foo\"bar''") `shouldBe` "\"foo\\\"bar\""
            exprIndent (parseStr "''foo\"bar''") `shouldBe` "\"foo\\\"bar\""
        it "indents lists" $ do
            exprIndent (parseStr "[1 2 3]") `shouldBe` "[1 2 3]"
        it "indents sets" $ do
            exprIndent (parseStr "{a=3;c=5;}") `shouldBe` "{ a = 3; c = 5; }"
            exprIndent (parseStr "{inherit foo;}") `shouldBe` "{ inherit foo; }"
            exprIndent (parseStr "{inherit (x) a b c;}") `shouldBe`
                "{ inherit (x) a b c; }"
            exprIndent (parseStr "{${a}=3;}") `shouldBe` "{ ${a} = 3; }"
            exprIndent (parseStr "{\"${a}\"=3;}") `shouldBe` "{ \"${a}\" = 3; }"
            exprIndent (parseStr "{\"a${a}b\"=3;}") `shouldBe`
                "{ \"a${a}b\" = 3; }"
        it "indents symbols" $ do
            exprIndent (parseStr "a-b") `shouldBe` "a-b"
            exprIndent (parseStr "{a=b;b=a;}") `shouldBe` "{ a = b; b = a; }"
        it "indents recursive sets" $ do
            exprIndent (parseStr "rec{a=b;b=a;}") `shouldBe`
                "rec { a = b; b = a; }"
        it "indents literal paths" $ do
            exprIndent (parseStr "./foo.bar") `shouldBe` "./foo.bar"
        it "indents environment paths" $ do
            exprIndent (parseStr "<nixpkgs/nixos>") `shouldBe` "<nixpkgs/nixos>"
        it "indents operator-based expressions" $ do
            exprIndent (parseStr "-(1+2+3-5*6 == 7 && 8 > 9)") `shouldBe`
                "-(((1 + 2) + 3) - 5 * 6 == 7 && 8 > 9)"
            exprIndent (parseStr "-3") `shouldBe` "-3"
        it "indents select expressions" $ do
            exprIndent (parseStr "({}.a or {b=1;}).b") `shouldBe`
                "({}.a or { b = 1; }).b"
            exprIndent (parseStr "{a=2;}.a or (3+3)") `shouldBe`
                "{ a = 2; }.a or (3 + 3)"
        it "indents ?-expressions" $ do
            exprIndent (parseStr "{a=1;}?a && {} ? b") `shouldBe`
                "{ a = 1; } ? a && {} ? b"
        it "indents lambdas" $ do
            exprIndent (parseStr "foo: bar: foo+bar") `shouldBe`
                "foo: bar: foo + bar"
        it "indents function applications" $ do
            exprIndent (parseStr "(foo: foo) 3") `shouldBe`
                "(foo: foo) 3"
        it "indents let expressions" $ do
            exprIndent (parseStr "let a=1; in a") `shouldBe`
                "let a = 1; in a"
        it "indents if-expressions" $ do
            exprIndent (parseStr "if  a==b then 0 else 1") `shouldBe`
                "if a == b then 0 else 1"
        it "indents with-blocks" $ do
            exprIndent (parseStr "with {}; 12") `shouldBe`
                "with {}; 12"
        it "indents assertions" $ do
            exprIndent (parseStr "assert 1==2; null") `shouldBe`
                "assert 1 == 2; null"
        it "indents parameter sets" $ do
            exprIndent (parseStr "{foo, bar?0, baz?{x=1;}}: baz") `shouldBe`
                "{ bar ? 0, baz ? { x = 1; }, foo }: baz"
            exprIndent (parseStr "{foo, bar?0, baz?{x=1;},...}: baz") `shouldBe`
                "{ bar ? 0, baz ? { x = 1; }, foo, ... }: baz"
