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
        it "indents lists" $ do
            exprIndent (parseStr "[1 2 3]") `shouldBe` "[1 2 3 ]"
        it "indents sets" $ do
            exprIndent (parseStr "{a=3;c=5;}") `shouldBe` "{a=3;c=5;}"
        it "indents symbols" $ do
            exprIndent (parseStr "a-b") `shouldBe` "a-b"
            exprIndent (parseStr "{a=b;b=a;}") `shouldBe` "{a=b;b=a;}"
        it "indents recursive sets" $ do
            exprIndent (parseStr "rec{a=b;b=a;}") `shouldBe` "rec {a=b;b=a;}"
        it "indents literal paths" $ do
            exprIndent (parseStr "./foo.bar") `shouldBe` "./foo.bar"
        it "indents environment paths" $ do
            exprIndent (parseStr "<nixpkgs/nixos>") `shouldBe` "<nixpkgs/nixos>"
