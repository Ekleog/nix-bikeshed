import Data.Fix
import qualified Data.Text as T

import Lib

import Nix.Atoms
import Nix.Expr

import Test.Hspec

lineIndentsTo :: String -> String -> Expectation
lineIndentsTo a b = do
    exprI (parseStr a) `shouldBe` b
    exprL (parseStr a) `shouldBe` length b

shouldBeEquivalentTo :: String -> String -> Expectation
shouldBeEquivalentTo a b =
    (parseStr a `isEquivalentTo` parseStr b) `shouldBe` True

shouldNotBeEquivalentTo :: String -> String -> Expectation
shouldNotBeEquivalentTo a b =
    (parseStr a `isEquivalentTo` parseStr b) `shouldBe` False

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

    describe "isEquivalentTo" $ do
        it "considers equivalent equal constants" $ do
            "42" `shouldBeEquivalentTo` "42"
        it "considers different different constants" $ do
            "42" `shouldNotBeEquivalentTo` "43"
        it "considers equivalent differently-quoted strings" $ do
            "''a''" `shouldBeEquivalentTo` "\"a\""
        it "considers different different strings" $ do
            "\"a${\"b\"}c\"" `shouldNotBeEquivalentTo` "\"a${\"d\"}c\""

    describe "exprI" $ do
        it "indents constants" $ do
            "42" `lineIndentsTo` "42"
            "true" `lineIndentsTo` "true"
            "false" `lineIndentsTo` "false"
            "null" `lineIndentsTo` "null"
            "https://example.org" `lineIndentsTo` "https://example.org"
        it "indents strings" $ do
            "\"foo bar\"" `lineIndentsTo` "\"foo bar\""
            "''foo bar''" `lineIndentsTo` "\"foo bar\""
            "''foo\"bar''" `lineIndentsTo` "\"foo\\\"bar\""
            "''foo\"bar''" `lineIndentsTo` "\"foo\\\"bar\""
        it "indents lists" $ do
            "[1 2 3]" `lineIndentsTo` "[1 2 3]"
        it "indents sets" $ do
            "{a=3;c=5;}" `lineIndentsTo` "{ a = 3; c = 5; }"
            "{inherit foo;}" `lineIndentsTo` "{ inherit foo; }"
            "{inherit (x) a b c;}" `lineIndentsTo` "{ inherit (x) a b c; }"
            "{${a}=3;}" `lineIndentsTo` "{ ${a} = 3; }"
            "{\"${a}\"=3;}" `lineIndentsTo` "{ \"${a}\" = 3; }"
            "{\"a${a}b\"=3;}" `lineIndentsTo` "{ \"a${a}b\" = 3; }"
        it "indents symbols" $ do
            "a-b" `lineIndentsTo` "a-b"
            "{a=b;b=a;}" `lineIndentsTo` "{ a = b; b = a; }"
        it "indents recursive sets" $ do
            "rec{a=b;b=a;}" `lineIndentsTo` "rec { a = b; b = a; }"
        it "indents literal paths" $ do
            "./foo.bar" `lineIndentsTo` "./foo.bar"
        it "indents environment paths" $ do
            "<nixpkgs/nixos>" `lineIndentsTo` "<nixpkgs/nixos>"
        it "indents operator-based expressions" $ do
            "-(1+2+3-5*6 == 7 && 8 > 9)" `lineIndentsTo`
                "-(1 + 2 + 3 - 5 * 6 == 7 && 8 > 9)"
            "-3" `lineIndentsTo` "-3"
            "1+2+(3+4)" `lineIndentsTo` "1 + 2 + 3 + 4"
            "1+2-(3+4)" `lineIndentsTo` "1 + 2 - (3 + 4)"
        it "indents select expressions" $ do
            "({}.a or {b=1;}).b" `lineIndentsTo` "({}.a or { b = 1; }).b"
            "{a=2;}.a or (3+3)" `lineIndentsTo` "{ a = 2; }.a or (3 + 3)"
        it "indents ?-expressions" $ do
            "{a=1;}?a && {} ? b" `lineIndentsTo` "{ a = 1; } ? a && {} ? b"
        it "indents lambdas" $ do
            "foo: bar: foo+bar" `lineIndentsTo` "foo: bar: foo + bar"
        it "indents inline function applications" $ do
            "(foo: foo) 3" `lineIndentsTo` "(foo: foo) 3"
        it "indents out-of-line function applications" $ do
            "let foo = x: (x +1); in foo  3" `lineIndentsTo`
                "let foo = x: x + 1; in foo 3"
        it "does not forget parenthesis around parameters" $ do
            "(x: x) (!true)" `lineIndentsTo` "(x: x) (!true)"
        it "indents let expressions" $ do
            "let a=1; in a" `lineIndentsTo` "let a = 1; in a"
        it "indents if-expressions" $ do
            "if  a==b then 0 else 1" `lineIndentsTo` "if a == b then 0 else 1"
        it "indents with-blocks" $ do
            "with {}; 12" `lineIndentsTo` "with {}; 12"
        it "indents assertions" $ do
            "assert 1==2; null" `lineIndentsTo` "assert 1 == 2; null"
        it "indents parameter sets" $ do
            "{foo, bar?0, baz?{x=1;}}: baz" `lineIndentsTo`
                "{ bar ? 0, baz ? { x = 1; }, foo }: baz"
        it "indents variadic parameter sets" $ do
            "{foo, bar?0, baz?{x=1;},...}: baz" `lineIndentsTo`
                "{ bar ? 0, baz ? { x = 1; }, foo, ... }: baz"
