module Lib (doTheThing) where

import Data.Fix
import Data.List
import qualified Data.Text as T

import Debug.Trace

import Nix.Atoms
import Nix.Expr
import Nix.Parser

doTheThing :: IO ()
doTheThing = do
    expr <- parse "test-input.nix"
    putStrLn $ indent expr

parse :: FilePath -> IO NExpr
parse f = do
    expr <- parseNixFile f
    case expr of
        Success a -> return a
        Failure e -> error $ show e

indent :: NExpr -> String
indent a = trace (show a) $ exprIndent a

exprIndent :: NExpr -> String
exprIndent expr = case expr of
    Fix (NConstant c) -> atomIndent c
    Fix (NStr s) -> stringIndent s
    Fix (NList vals) -> "[" ++ concatMap (\x -> exprIndent x ++ " ") vals ++ "]"
    Fix (NSet binds) -> "{" ++ concatMap bindingIndent binds ++ "}"
    _ -> "non implemented"

bindingIndent :: Binding NExpr -> String
bindingIndent b = case b of
    NamedVar path val -> pathIndent path ++ "=" ++ exprIndent val ++ ";"
    Inherit _ _ -> "non implemented"

pathIndent :: NAttrPath NExpr -> String
pathIndent p = concat $ intersperse "." $ map keyNameIndent p

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
