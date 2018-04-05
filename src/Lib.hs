module Lib where

import Data.Fix
import Data.List
import qualified Data.Text as T

import Debug.Trace

import Nix.Atoms
import Nix.Expr
import Nix.Parser

doTheThing :: IO ()
doTheThing = do
    expr <- parseFile "test-input.nix"
    putStrLn $ indent expr

parseFile :: FilePath -> IO NExpr
parseFile f = do
    expr <- parseNixFile f
    case expr of
        Success a -> return a
        Failure e -> error $ show e

parseStr :: String -> NExpr
parseStr s = case parseNixString s of
    Success a -> a
    Failure e -> error $ show e

indent :: NExpr -> String
indent a = trace (show a) $ exprIndent a

exprIndent :: NExpr -> String
exprIndent expr = case expr of
    Fix (NConstant c) -> atomIndent c
    Fix (NStr s) -> stringIndent s
    Fix (NSym s) -> T.unpack s
    Fix (NList vals) -> "[" ++ intercalate " " (map exprIndent vals) ++ "]"
    Fix (NSet binds) -> if binds == [] then "{}"
                        else "{ " ++ intercalate " " (map bindingIndent binds) ++ " }"
    Fix (NRecSet binds) -> if binds == [] then "rec {}"
                           else "rec { " ++ intercalate " " (map bindingIndent binds) ++ " }"
    Fix (NLiteralPath p) -> p
    Fix (NEnvPath p) -> "<" ++ p ++ ">"
    Fix (NUnary op ex) -> unaryOpIndent op ex
    Fix (NBinary op l r) -> binaryOpIndent op l r
    Fix (NSelect set attr def) -> selectIndent set attr def
    Fix (NHasAttr _ _) -> "non implemented"
    Fix (NAbs _ _) -> "non implemented"
    Fix (NApp _ _) -> "non implemented"
    Fix (NLet _ _) -> "non implemented"
    Fix (NIf _ _ _) -> "non implemented"
    Fix (NWith _ _) -> "non implemented"
    Fix (NAssert _ _) -> "non implemented"

bindingIndent :: Binding NExpr -> String
bindingIndent b = case b of
    NamedVar path val -> pathIndent path ++ " = " ++ exprIndent val ++ ";"
    Inherit _ _ -> "non implemented"

pathIndent :: NAttrPath NExpr -> String
pathIndent p = intercalate "." $ map keyNameIndent p

keyNameIndent :: NKeyName NExpr -> String
keyNameIndent kn = case kn of
    DynamicKey _ -> "non implemented"
    StaticKey k -> T.unpack k

atomIndent :: NAtom -> String
atomIndent a = case a of
    NInt i -> show i
    NBool True -> "true"
    NBool False -> "false"
    NNull -> "null"
    NUri t -> T.unpack t

stringIndent :: NString NExpr -> String
stringIndent s = case s of
    DoubleQuoted t -> stringIndent (Indented t) -- ignore string type
    Indented t -> "\"" ++ concatMap antiquotedIndent t ++ "\""

-- TODO: escape the text
antiquotedIndent :: Antiquoted T.Text NExpr -> String
antiquotedIndent a = case a of
    Plain t -> T.unpack t
    Antiquoted e -> "non implemented"

unaryOpIndent :: NUnaryOp -> NExpr -> String
unaryOpIndent op ex = case op of
    NNeg -> "-(" ++ exprIndent ex ++ ")"
    NNot -> "!(" ++ exprIndent ex ++ ")"

binaryOpIndent :: NBinaryOp -> NExpr -> NExpr -> String
binaryOpIndent op l r = case op of
    NEq -> "(" ++ exprIndent l ++ " == " ++ exprIndent r ++ ")"
    NNEq -> "(" ++ exprIndent l ++ " != " ++ exprIndent r ++ ")"
    NLt -> "(" ++ exprIndent l ++ " < " ++ exprIndent r ++ ")"
    NLte -> "(" ++ exprIndent l ++ " <= " ++ exprIndent r ++ ")"
    NGt -> "(" ++ exprIndent l ++ " > " ++ exprIndent r ++ ")"
    NGte -> "(" ++ exprIndent l ++ " >= " ++ exprIndent r ++ ")"
    NAnd -> "(" ++ exprIndent l ++ " && " ++ exprIndent r ++ ")"
    NOr -> "(" ++ exprIndent l ++ " || " ++ exprIndent r ++ ")"
    NImpl -> "(" ++ exprIndent l ++ " -> " ++ exprIndent r ++ ")"
    NUpdate -> "(" ++ exprIndent l ++ " // " ++ exprIndent r ++ ")"
    NPlus -> "(" ++ exprIndent l ++ " + " ++ exprIndent r ++ ")"
    NMinus -> "(" ++ exprIndent l ++ " - " ++ exprIndent r ++ ")"
    NMult -> "(" ++ exprIndent l ++ " * " ++ exprIndent r ++ ")"
    NDiv -> "(" ++ exprIndent l ++ " / " ++ exprIndent r ++ ")"
    NConcat -> "(" ++ exprIndent l ++ " ++ " ++ exprIndent r ++ ")"

selectIndent :: NExpr -> NAttrPath NExpr -> Maybe NExpr -> String
selectIndent set attr def =
    "(" ++ exprIndent set ++ ")." ++ pathIndent attr ++
        maybe "" (\x -> " or " ++ exprIndent x) def
