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

    describe "exprI" $ do
        it "indents constants" $ do
            exprI (parseStr "42") `shouldBe` "42"
            exprI (parseStr "true") `shouldBe` "true"
            exprI (parseStr "false") `shouldBe` "false"
            exprI (parseStr "null") `shouldBe` "null"
            exprI (parseStr "https://example.org") `shouldBe`
                "https://example.org"
        it "indents strings" $ do
            exprI (parseStr "\"foo bar\"") `shouldBe` "\"foo bar\""
            exprI (parseStr "''foo bar''") `shouldBe` "\"foo bar\""
            exprI (parseStr "''foo\"bar''") `shouldBe` "\"foo\\\"bar\""
            exprI (parseStr "''foo\"bar''") `shouldBe` "\"foo\\\"bar\""
        it "indents lists" $ do
            exprI (parseStr "[1 2 3]") `shouldBe` "[1 2 3]"
        it "indents sets" $ do
            exprI (parseStr "{a=3;c=5;}") `shouldBe` "{ a = 3; c = 5; }"
            exprI (parseStr "{inherit foo;}") `shouldBe` "{ inherit foo; }"
            exprI (parseStr "{inherit (x) a b c;}") `shouldBe`
                "{ inherit (x) a b c; }"
            exprI (parseStr "{${a}=3;}") `shouldBe` "{ ${a} = 3; }"
            exprI (parseStr "{\"${a}\"=3;}") `shouldBe` "{ \"${a}\" = 3; }"
            exprI (parseStr "{\"a${a}b\"=3;}") `shouldBe`
                "{ \"a${a}b\" = 3; }"
        it "indents symbols" $ do
            exprI (parseStr "a-b") `shouldBe` "a-b"
            exprI (parseStr "{a=b;b=a;}") `shouldBe` "{ a = b; b = a; }"
        it "indents recursive sets" $ do
            exprI (parseStr "rec{a=b;b=a;}") `shouldBe`
                "rec { a = b; b = a; }"
        it "indents literal paths" $ do
            exprI (parseStr "./foo.bar") `shouldBe` "./foo.bar"
        it "indents environment paths" $ do
            exprI (parseStr "<nixpkgs/nixos>") `shouldBe` "<nixpkgs/nixos>"
        it "indents operator-based expressions" $ do
            exprI (parseStr "-(1+2+3-5*6 == 7 && 8 > 9)") `shouldBe`
                "-(1 + 2 + 3 - 5 * 6 == 7 && 8 > 9)"
            exprI (parseStr "-3") `shouldBe` "-3"
            exprI (parseStr "1+2+(3+4)") `shouldBe` "1 + 2 + 3 + 4"
            exprI (parseStr "1+2-(3+4)") `shouldBe` "1 + 2 - (3 + 4)"
        it "indents select expressions" $ do
            exprI (parseStr "({}.a or {b=1;}).b") `shouldBe`
                "({}.a or { b = 1; }).b"
            exprI (parseStr "{a=2;}.a or (3+3)") `shouldBe`
                "{ a = 2; }.a or (3 + 3)"
        it "indents ?-expressions" $ do
            exprI (parseStr "{a=1;}?a && {} ? b") `shouldBe`
                "{ a = 1; } ? a && {} ? b"
        it "indents lambdas" $ do
            exprI (parseStr "foo: bar: foo+bar") `shouldBe`
                "foo: bar: foo + bar"
        it "indents function applications" $ do
            exprI (parseStr "(foo: foo) 3") `shouldBe`
                "(foo: foo) 3"
            exprI (parseStr "let foo = x: (x +1); in foo  3") `shouldBe`
                "let foo = x: x + 1; in foo 3"
        it "indents let expressions" $ do
            exprI (parseStr "let a=1; in a") `shouldBe`
                "let a = 1; in a"
        it "indents if-expressions" $ do
            exprI (parseStr "if  a==b then 0 else 1") `shouldBe`
                "if a == b then 0 else 1"
        it "indents with-blocks" $ do
            exprI (parseStr "with {}; 12") `shouldBe`
                "with {}; 12"
        it "indents assertions" $ do
            exprI (parseStr "assert 1==2; null") `shouldBe`
                "assert 1 == 2; null"
        it "indents parameter sets" $ do
            exprI (parseStr "{foo, bar?0, baz?{x=1;}}: baz") `shouldBe`
                "{ bar ? 0, baz ? { x = 1; }, foo }: baz"
            exprI (parseStr "{foo, bar?0, baz?{x=1;},...}: baz") `shouldBe`
                "{ bar ? 0, baz ? { x = 1; }, foo, ... }: baz"
