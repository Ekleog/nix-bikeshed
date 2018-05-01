import Data.Fix
import qualified Data.Text as T

import Lib

import Nix.Atoms
import Nix.Expr

import Test.Hspec

lineIndentsTo :: String -> String -> Expectation
lineIndentsTo a b = do
    indentExpr 1000000000 (parseStr a) `shouldBe` b
    exprL (parseStr a) `shouldBe` length b

shortIndentsTo :: String -> String -> Expectation
shortIndentsTo a b = indentExpr 0 (parseStr a) `shouldBe` b

indentsTo :: Int -> String -> String -> Expectation
indentsTo l a b = indentExpr l (parseStr a) `shouldBe` b

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

    describe "indentExpr" $ do
        it "outputs constants" $ do
            "42" `lineIndentsTo` "42"
            "true" `lineIndentsTo` "true"
            "false" `lineIndentsTo` "false"
            "null" `lineIndentsTo` "null"
            "https://example.org" `lineIndentsTo` "https://example.org"
        it "outputs strings" $ do
            "\"foo bar\"" `lineIndentsTo` "\"foo bar\""
            "''foo bar''" `lineIndentsTo` "\"foo bar\""
            "''foo\"bar''" `lineIndentsTo` "\"foo\\\"bar\""
            "''foo\"bar''" `lineIndentsTo` "\"foo\\\"bar\""
        it "outputs lists" $ do
            "[1 2 3]" `lineIndentsTo` "[1 2 3]"
        it "does not forget parenthesis  in lists" $ do
            "[1 (1+1) 3]" `lineIndentsTo` "[1 (1 + 1) 3]"
        it "outputs sets" $ do
            "{a=3;c=5;}" `lineIndentsTo` "{ a = 3; c = 5; }"
            "{inherit foo;}" `lineIndentsTo` "{ inherit foo; }"
            "{inherit (x) a b c;}" `lineIndentsTo` "{ inherit (x) a b c; }"
            "{${a}=3;}" `lineIndentsTo` "{ ${a} = 3; }"
            "{\"${a}\"=3;}" `lineIndentsTo` "{ \"${a}\" = 3; }"
            "{\"a${a}b\"=3;}" `lineIndentsTo` "{ \"a${a}b\" = 3; }"
        it "outputs symbols" $ do
            "a-b" `lineIndentsTo` "a-b"
            "{a=b;b=a;}" `lineIndentsTo` "{ a = b; b = a; }"
        it "outputs recursive sets" $ do
            "rec{a=b;b=a;}" `lineIndentsTo` "rec { a = b; b = a; }"
        it "outputs literal paths" $ do
            "./foo.bar" `lineIndentsTo` "./foo.bar"
        it "outputs environment paths" $ do
            "<nixpkgs/nixos>" `lineIndentsTo` "<nixpkgs/nixos>"
        it "outputs operator-based expressions" $ do
            "-(1+2+3-5*6 == 7 && 8 > 9)" `lineIndentsTo`
                "-(1 + 2 + 3 - 5 * 6 == 7 && 8 > 9)"
            "-3" `lineIndentsTo` "-3"
            "1+2+(3+4)" `lineIndentsTo` "1 + 2 + 3 + 4"
            "1+2-(3+4)" `lineIndentsTo` "1 + 2 - (3 + 4)"
        it "doesn't forget parenthesis around unary operator in function application" $ do
            "let foo = a: a + 1; in foo (- 1)" `lineIndentsTo`
                "let foo = a: a + 1; in foo (-1)"
        it "doesn't forget chaining update operators" $ do
            "a//b//c" `lineIndentsTo` "a // b // c"
        it "outputs select expressions" $ do
            "({}.a or {b=1;}).b" `lineIndentsTo` "({}.a or { b = 1; }).b"
            "{a=2;}.a or (3+3)" `lineIndentsTo` "{ a = 2; }.a or (3 + 3)"
        it "outputs ?-expressions" $ do
            "{a=1;}?a && {} ? b" `lineIndentsTo` "{ a = 1; } ? a && {} ? b"
        it "outputs lambdas" $ do
            "foo: bar: foo+bar" `lineIndentsTo` "foo: bar: foo + bar"
        it "outputs inline function applications" $ do
            "(foo: foo) 3" `lineIndentsTo` "(foo: foo) 3"
        it "outputs out-of-line function applications" $ do
            "let foo = x: (x +1); in foo  3" `lineIndentsTo`
                "let foo = x: x + 1; in foo 3"
        it "considers function application left-associative" $ do
            "a (b 2)" `lineIndentsTo` "a (b 2)"
        it "does not forget parenthesis around parameters" $ do
            "(x: x) (!true)" `lineIndentsTo` "(x: x) (!true)"
        it "outputs let expressions" $ do
            "let a=1; in a" `lineIndentsTo` "let a = 1; in a"
        it "outputs if-expressions" $ do
            "if  a==b then 0 else 1" `lineIndentsTo` "if a == b then 0 else 1"
        it "outputs with-blocks" $ do
            "with {}; 12" `lineIndentsTo` "with {}; 12"
        it "outputs assertions" $ do
            "assert 1==2; null" `lineIndentsTo` "assert 1 == 2; null"
        it "outputs parameter sets" $ do
            "{foo, bar?0, baz?{x=1;}}: baz" `lineIndentsTo`
                "{ bar ? 0, baz ? { x = 1; }, foo }: baz"
        it "outputs named parameter sets" $ do
            "{b}@a:b" `lineIndentsTo` "{ b } @ a: b"
        it "outputs variadic parameter sets" $ do
            "{foo, bar?0, baz?{x=1;},...}: baz" `lineIndentsTo`
                "{ bar ? 0, baz ? { x = 1; }, foo, ... }: baz"

        it "indents sets" $ do
            "{foo=bar;baz=quux;}" `shortIndentsTo` "{\n\
            \  foo = bar;\n\
            \  baz = quux;\n\
            \}"
        it "indents function definitions" $ do
            "a: b" `shortIndentsTo` "a: b"
        it "indents parameter sets" $ do
            "{a?b,c}:d" `shortIndentsTo` "{\n\
            \  a ? b,\n\
            \  c\n\
            \}: d"
            "{a,...}:b" `shortIndentsTo` "{\n\
            \  a,\n\
            \  ...\n\
            \}: b"
        it "indents let blocks" $ do
            "let a=b; in c" `shortIndentsTo` "let\n\
            \  a = b;\n\
            \in\n\
            \c"
        it "indents strings" $ do
            "\"a\n${b}\nc\"" `shortIndentsTo` "''\n\
            \  a\n\
            \  ${b}\n\
            \  c''"
            "\"a\n\"" `shortIndentsTo` "''\n\
            \  a\n\
            \''"
        it "indents function definitions" $ do
            indentsTo 10 "foo: bar: { baz = foo; }" "foo: bar: {\n\
            \  baz = foo;\n\
            \}"
