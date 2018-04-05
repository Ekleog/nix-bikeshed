module Lib where

import Data.Fix
import Data.List
import qualified Data.Map as M
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

-- TODO: handle priority of operators
exprIndent :: NExpr -> String
exprIndent expr = case expr of
    Fix (NConstant c) -> atomIndent c
    Fix (NStr s) -> stringIndent s
    Fix (NSym s) -> T.unpack s
    Fix (NList vals) -> "[" ++ intercalate " " (map exprIndent vals) ++ "]"
    Fix (NSet binds) -> setIndent False binds
    Fix (NRecSet binds) -> setIndent True binds
    Fix (NLiteralPath p) -> p
    Fix (NEnvPath p) -> "<" ++ p ++ ">"
    Fix (NUnary op ex) -> unaryOpIndent op ex
    Fix (NBinary op l r) -> binaryOpIndent op l r
    Fix (NSelect set attr def) -> selectIndent set attr def
    Fix (NHasAttr set attr) -> hasAttrIndent set attr
    Fix (NAbs param expr) -> absIndent param expr
    Fix (NApp f x) -> appIndent f x
    Fix (NLet binds ex) -> letIndent binds ex
    Fix (NIf cond then_ else_) -> ifIndent cond then_ else_
    Fix (NWith set expr) -> stmtIndent "with" set expr
    Fix (NAssert test expr) -> stmtIndent "assert" test expr

-- Less binds stronger
exprPrio :: NExpr -> Int
exprPrio expr = case expr of
    -- See https://nixos.org/nix/manual/#table-operators
    Fix (NConstant _) -> 0
    Fix (NStr _) -> 0
    Fix (NSym _) -> 0
    Fix (NList _) -> 0
    Fix (NSet _) -> 0
    Fix (NRecSet _) -> 0
    Fix (NLiteralPath _) -> 0
    Fix (NEnvPath _) -> 0
    Fix (NSelect _ _ _) -> selectPrio
    Fix (NApp _ _) -> appPrio
    Fix (NUnary op _) -> unaryPrio op
    Fix (NHasAttr _ _) -> hasAttrPrio
    Fix (NBinary op _ _) -> binaryPrio op
    -- No actual priority issue on these, they bind less
    Fix (NAbs _ _) -> 100
    Fix (NLet _ _) -> 100
    Fix (NIf _ _ _) -> 100
    Fix (NWith _ _) -> 100
    Fix (NAssert _ _) -> 100

selectPrio :: Int
selectPrio = 1

appPrio :: Int
appPrio = 2

unaryPrio :: NUnaryOp -> Int
unaryPrio op = case op of
    NNeg -> 3
    NNot -> 8

hasAttrPrio :: Int
hasAttrPrio = 4

binaryPrio :: NBinaryOp -> Int
binaryPrio op = case op of
    NConcat -> 5
    NMult -> 6
    NDiv -> 6
    NPlus -> 7
    NMinus -> 7
    NUpdate -> 9
    NLt -> 10
    NLte -> 10
    NGt -> 10
    NGte -> 10
    NEq -> 11
    NNEq -> 11
    NAnd -> 12
    NOr -> 13
    NImpl -> 14

paren :: String -> String
paren s = "(" ++ s ++ ")"

parenIf :: Bool -> String -> String
parenIf cond = if cond then paren else id

bindingIndent :: Binding NExpr -> String
bindingIndent b = case b of
    NamedVar path val -> pathIndent path ++ " = " ++ exprIndent val ++ ";"
    Inherit Nothing vars ->
        "inherit " ++ intercalate " " (map keyNameIndent vars) ++ ";"
    Inherit (Just s) vars ->
        "inherit (" ++ exprIndent s ++ ") " ++
            intercalate " " (map keyNameIndent vars) ++ ";"

pathIndent :: NAttrPath NExpr -> String
pathIndent p = intercalate "." $ map keyNameIndent p

keyNameIndent :: NKeyName NExpr -> String
keyNameIndent kn = case kn of
    StaticKey k -> T.unpack k
    DynamicKey (Plain s) -> stringIndent s
    DynamicKey (Antiquoted e) -> "${" ++ exprIndent e ++ "}"

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
    Indented t -> "\"" ++ concatMap escapeAntiquoted t ++ "\""

escapeAntiquoted :: Antiquoted T.Text NExpr -> String
escapeAntiquoted a = case a of
    Plain t -> tail $ init $ show $ T.unpack t
    Antiquoted e -> "${" ++ exprIndent e ++ "}"

unOpStr :: NUnaryOp -> String
unOpStr op = case op of
    NNeg -> "-"
    NNot -> "!"

unaryOpIndent :: NUnaryOp -> NExpr -> String
unaryOpIndent op ex =
    unOpStr op ++ parenIf (unaryPrio op < exprPrio ex) (exprIndent ex)

binOpStr :: NBinaryOp -> String
binOpStr op = case op of
    NEq -> "=="
    NNEq -> "!="
    NLt -> "<"
    NLte -> "<="
    NGt -> ">"
    NGte -> ">="
    NAnd -> "&&"
    NOr -> "||"
    NImpl -> "->"
    NUpdate -> "//"
    NPlus -> "+"
    NMinus -> "-"
    NMult -> "*"
    NDiv -> "/"
    NConcat -> "++"

binaryOpIndent :: NBinaryOp -> NExpr -> NExpr -> String
binaryOpIndent op l r =
    parenIf (binaryPrio op <= exprPrio l) (exprIndent l) ++ " " ++
    binOpStr op ++ " " ++
    parenIf (binaryPrio op <= exprPrio r) (exprIndent r)

selectIndent :: NExpr -> NAttrPath NExpr -> Maybe NExpr -> String
selectIndent set attr def =
    parenIf (selectPrio <= exprPrio set) (exprIndent set) ++
    "." ++ pathIndent attr ++
    maybe ""
          (\x -> " or " ++ parenIf (selectPrio <= exprPrio x) (exprIndent x))
          def

hasAttrIndent :: NExpr -> NAttrPath NExpr -> String
hasAttrIndent set attr =
    parenIf (hasAttrPrio <= exprPrio set) (exprIndent set) ++
    " ? " ++ pathIndent attr

absIndent :: Params NExpr -> NExpr -> String
absIndent par ex = paramIndent par ++ ": " ++ exprIndent ex

paramIndent :: Params NExpr -> String
paramIndent par = case par of
    Param p -> T.unpack p
    ParamSet set Nothing -> paramSetIndent set
    ParamSet set (Just n) -> paramSetIndent set ++ " @ " ++ T.unpack n

paramSetIndent :: ParamSet NExpr -> String
paramSetIndent set = case set of
    FixedParamSet m -> "{ " ++ paramSetIndentImpl m ++ " }"
    VariadicParamSet m -> "{ " ++ paramSetIndentImpl m ++ ", ... }"

paramSetIndentImpl :: M.Map T.Text (Maybe NExpr) -> String
paramSetIndentImpl set =
    intercalate ", " (
        map (\(k, x) -> case x of
            Nothing -> T.unpack k
            Just e -> T.unpack k ++ " ? " ++ exprIndent e
        ) (M.toList set)
    )

appIndent :: NExpr -> NExpr -> String
appIndent f x =
    parenIf (appPrio < exprPrio f) (exprIndent f) ++ " " ++ exprIndent x

setIndent :: Bool -> [Binding NExpr] -> String
setIndent rec binds =
    (if rec then "rec " else "") ++
    (if binds == [] then "{}"
     else "{ " ++ intercalate " " (map bindingIndent binds) ++ " }")

letIndent :: [Binding NExpr] -> NExpr -> String
letIndent binds ex =
    "let " ++ intercalate " " (map bindingIndent binds) ++ " in " ++
    exprIndent ex

ifIndent :: NExpr -> NExpr -> NExpr -> String
ifIndent cond then_ else_ =
    "if " ++ exprIndent cond ++ " then " ++ exprIndent then_ ++
    " else " ++ exprIndent else_;

stmtIndent :: String -> NExpr -> NExpr -> String
stmtIndent kw it expr = kw ++ " " ++ exprIndent it ++ "; " ++ exprIndent expr
