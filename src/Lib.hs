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
indent a = trace (show a) $ exprI a

-- *I functions return the string that fits the constraints
exprI :: NExpr -> String
exprI expr = case expr of
    Fix (NConstant c) -> atomI c
    Fix (NStr s) -> stringI s
    Fix (NSym s) -> T.unpack s
    Fix (NList vals) -> "[" ++ intercalate " " (map exprI vals) ++ "]"
    Fix (NSet binds) -> setI False binds
    Fix (NRecSet binds) -> setI True binds
    Fix (NLiteralPath p) -> p
    Fix (NEnvPath p) -> "<" ++ p ++ ">"
    Fix (NUnary op ex) -> unaryOpI op ex
    Fix (NBinary op l r) -> binaryOpI op l r
    Fix (NSelect set attr def) -> selectI set attr def
    Fix (NHasAttr set attr) -> hasAttrI set attr
    Fix (NAbs param expr) -> absI param expr
    Fix (NApp f x) -> appI f x
    Fix (NLet binds ex) -> letI binds ex
    Fix (NIf cond then_ else_) -> ifI cond then_ else_
    Fix (NWith set expr) -> stmtI "with" set expr
    Fix (NAssert test expr) -> stmtI "assert" test expr

-- *L functions return the length the expression would take if put all on one
-- line
exprL :: NExpr -> Int
exprL expr = case expr of
    Fix (NConstant c) -> atomL c
    Fix (NStr s) -> stringL s
    Fix (NSym s) -> T.length s
    Fix (NList vals) -> 1 + length vals + sum (map exprL vals)
    Fix (NSet binds) -> setL False binds
    Fix (NRecSet binds) -> setL True binds
    Fix (NLiteralPath p) -> length p
    Fix (NEnvPath p) -> 2 + length p
    Fix (NUnary op ex) -> unaryOpL op ex
    Fix (NBinary op l r) -> binaryOpL op l r
    Fix (NSelect set attr def) -> selectL set attr def
    Fix (NHasAttr set attr) -> hasAttrL set attr
    Fix (NAbs param expr) -> absL param expr
    Fix (NApp f x) -> appL f x
    Fix (NLet binds ex) -> letL binds ex
    Fix (NIf cond then_ else_) -> ifL cond then_ else_
    Fix (NWith set expr) -> stmtL 4 set expr
    Fix (NAssert test expr) -> stmtL 6 test expr

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

associates :: NBinaryOp -> NBinaryOp -> Bool
associates l r = case (l, r) of
    -- *, /
    (NMult, NMult) -> True
    (NMult, NDiv) -> True
    (NDiv, NMult) -> False
    (NDiv, NDiv) -> False
    -- +, -
    (NPlus, NPlus) -> True
    (NPlus, NMinus) -> True
    (NMinus, NPlus) -> False
    (NMinus, NMinus) -> False
    -- ==, !=
    (NEq, NEq) -> False
    (NEq, NNEq) -> False
    (NNEq, NEq) -> False
    (NNEq, NNEq) -> False

paren :: String -> String
paren s = "(" ++ s ++ ")"

parenIf :: Bool -> String -> String
parenIf cond = if cond then paren else id

bindingI :: Binding NExpr -> String
bindingI b = case b of
    NamedVar path val -> pathI path ++ " = " ++ exprI val ++ ";"
    Inherit set vars ->
        "inherit " ++ maybe "" (\s -> "(" ++ exprI s ++ ") ") set ++
            intercalate " " (map keyNameI vars) ++ ";"

bindingL :: Binding NExpr -> Int
bindingL b = case b of
    NamedVar path val -> 4 + pathL path + exprL val
    Inherit set vars ->
        8 + length vars +
        maybe 0 (\s -> 3 + exprL s) set + sum (map keyNameL vars)

pathI :: NAttrPath NExpr -> String
pathI p = intercalate "." $ map keyNameI p

pathL :: NAttrPath NExpr -> Int
pathL p = length p - 1 + sum (map keyNameL p)

keyNameI :: NKeyName NExpr -> String
keyNameI kn = case kn of
    StaticKey k -> T.unpack k
    DynamicKey (Plain s) -> stringI s
    DynamicKey (Antiquoted e) -> "${" ++ exprI e ++ "}"

keyNameL :: NKeyName NExpr -> Int
keyNameL kn = case kn of
    StaticKey k -> T.length k
    DynamicKey (Plain s) -> stringL s
    DynamicKey (Antiquoted e) -> 3 + exprL e

atomI :: NAtom -> String
atomI a = case a of
    NInt i -> show i
    NBool True -> "true"
    NBool False -> "false"
    NNull -> "null"
    NUri t -> T.unpack t

atomL :: NAtom -> Int
atomL a = case a of
    NInt i -> length $ show i
    NBool True -> 4
    NBool False -> 5
    NNull -> 4
    NUri t -> T.length t

stringI :: NString NExpr -> String
stringI s = case s of
    DoubleQuoted t -> stringI (Indented t) -- ignore string type
    Indented t -> "\"" ++ concatMap escapeAntiquotedI t ++ "\""

stringL :: NString NExpr -> Int
stringL s = case s of
    DoubleQuoted t -> stringL (Indented t) -- ignore string type
    Indented t -> 2 + sum (map escapeAntiquotedL t)

escapeAntiquotedI :: Antiquoted T.Text NExpr -> String
escapeAntiquotedI a = case a of
    Plain t -> tail $ init $ show $ T.unpack t
    Antiquoted e -> "${" ++ exprI e ++ "}"

escapeAntiquotedL :: Antiquoted T.Text NExpr -> Int
escapeAntiquotedL a = case a of
    Plain t -> length (show $ T.unpack t) - 2
    Antiquoted e -> 3 + exprL e

unOpStr :: NUnaryOp -> String
unOpStr op = case op of
    NNeg -> "-"
    NNot -> "!"

unaryOpI :: NUnaryOp -> NExpr -> String
unaryOpI op ex =
    unOpStr op ++ parenIf (unaryPrio op < exprPrio ex) (exprI ex)

unaryOpL :: NUnaryOp -> NExpr -> Int
unaryOpL op ex =
    length (unOpStr op) + exprL ex +
    (if unaryPrio op < exprPrio ex then 2 else 0)

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

binOpOf :: NExpr -> NBinaryOp
binOpOf (Fix (NBinary op _ _)) = op

binaryOpNeedsParen :: Bool -> NBinaryOp -> NExpr -> Bool
binaryOpNeedsParen isLeftChild par child =
    binaryPrio par < exprPrio child || (
        binaryPrio par == exprPrio child &&
        not ((if isLeftChild then id else flip) associates (binOpOf child) par)
    )

binaryOpI :: NBinaryOp -> NExpr -> NExpr -> String
binaryOpI op l r =
    parenIf (binaryOpNeedsParen True op l) (exprI l) ++ " " ++
    binOpStr op ++ " " ++
    parenIf (binaryOpNeedsParen False op r) (exprI r)

binaryOpL :: NBinaryOp -> NExpr -> NExpr -> Int
binaryOpL op l r =
    exprL l + exprL r + length (binOpStr op) + 2 +
    (if binaryOpNeedsParen True op l then 2 else 0) +
    (if binaryOpNeedsParen False op r then 2 else 0)

selectI :: NExpr -> NAttrPath NExpr -> Maybe NExpr -> String
selectI set attr def =
    parenIf (selectPrio <= exprPrio set) (exprI set) ++
    "." ++ pathI attr ++
    maybe ""
          (\x -> " or " ++ parenIf (selectPrio <= exprPrio x) (exprI x))
          def

selectL :: NExpr -> NAttrPath NExpr -> Maybe NExpr -> Int
selectL set attr def =
    1 + (if selectPrio <= exprPrio set then 2 else 0) + exprL set + pathL attr +
    maybe 0
          (\x -> 4 + (if (selectPrio <= exprPrio x) then 2 else 0) + exprL x)
          def

hasAttrI :: NExpr -> NAttrPath NExpr -> String
hasAttrI set attr =
    parenIf (hasAttrPrio <= exprPrio set) (exprI set) ++
    " ? " ++ pathI attr

hasAttrL :: NExpr -> NAttrPath NExpr -> Int
hasAttrL set attr =
    3 + (if hasAttrPrio < exprPrio set then 2 else 0) + exprL set + pathL attr

absI :: Params NExpr -> NExpr -> String
absI par ex = paramI par ++ ": " ++ exprI ex

absL :: Params NExpr -> NExpr -> Int
absL par ex = paramL par + 2 + exprL ex

paramI :: Params NExpr -> String
paramI par = case par of
    Param p -> T.unpack p
    ParamSet set name -> paramSetI set ++ maybe "" T.unpack name

paramL :: Params NExpr -> Int
paramL par = case par of
    Param p -> T.length p
    ParamSet set name -> paramSetL set + maybe 0 (\n -> 3 + T.length n) name

paramSetI :: ParamSet NExpr -> String
paramSetI set = case set of
        FixedParamSet m -> "{ " ++ impl m ++ " }"
        VariadicParamSet m -> "{ " ++ impl m ++ ", ... }"
    where
        impl set =
            intercalate ", " (map (\(k, x) ->
                T.unpack k ++ maybe "" (\e -> " ? " ++ exprI e) x
            ) (M.toList set))

paramSetL :: ParamSet NExpr -> Int
paramSetL set = case set of
        FixedParamSet m -> 4 + impl (M.toList m)
        VariadicParamSet m -> 9 + impl (M.toList m)
    where
        impl l =
            2 * (length l - 1) +
            sum (map (
                \(k, x) -> T.length k + maybe 0 (\e -> 3 + exprL e) x
            ) l)

appI :: NExpr -> NExpr -> String
appI f x =
    parenIf (appPrio < exprPrio f) (exprI f) ++ " " ++ exprI x

appL :: NExpr -> NExpr -> Int
appL f x = (if appPrio < exprPrio f then 2 else 0) + exprL f + 1 + exprL x

setI :: Bool -> [Binding NExpr] -> String
setI rec binds =
    (if rec then "rec " else "") ++
    (if binds == [] then "{}"
     else "{ " ++ intercalate " " (map bindingI binds) ++ " }")

setL :: Bool -> [Binding NExpr] -> Int
setL rec binds =
    (if rec then 4 else 0) +
    (if binds == [] then 2 else 3 + length binds + sum (map bindingL binds))

letI :: [Binding NExpr] -> NExpr -> String
letI binds ex =
    "let " ++ intercalate " " (map bindingI binds) ++ " in " ++
    exprI ex

letL :: [Binding NExpr] -> NExpr -> Int
letL binds ex = 7 + length binds + exprL ex + sum (map bindingL binds)

ifI :: NExpr -> NExpr -> NExpr -> String
ifI cond then_ else_ =
    "if " ++ exprI cond ++ " then " ++ exprI then_ ++
    " else " ++ exprI else_;

ifL :: NExpr -> NExpr -> NExpr -> Int
ifL c t e = 15 + exprL c + exprL t + exprL e

stmtI :: String -> NExpr -> NExpr -> String
stmtI kw it expr = kw ++ " " ++ exprI it ++ "; " ++ exprI expr

stmtL :: Int -> NExpr -> NExpr -> Int
stmtL kwlen it expr = kwlen + 3 + exprL it + exprL expr
