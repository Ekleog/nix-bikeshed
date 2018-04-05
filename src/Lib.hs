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
    Inherit Nothing vars ->
        "inherit " ++ intercalate " " (map keyNameI vars) ++ ";"
    Inherit (Just s) vars ->
        "inherit (" ++ exprI s ++ ") " ++
            intercalate " " (map keyNameI vars) ++ ";"

pathI :: NAttrPath NExpr -> String
pathI p = intercalate "." $ map keyNameI p

keyNameI :: NKeyName NExpr -> String
keyNameI kn = case kn of
    StaticKey k -> T.unpack k
    DynamicKey (Plain s) -> stringI s
    DynamicKey (Antiquoted e) -> "${" ++ exprI e ++ "}"

atomI :: NAtom -> String
atomI a = case a of
    NInt i -> show i
    NBool True -> "true"
    NBool False -> "false"
    NNull -> "null"
    NUri t -> T.unpack t

stringI :: NString NExpr -> String
stringI s = case s of
    DoubleQuoted t -> stringI (Indented t) -- ignore string type
    Indented t -> "\"" ++ concatMap escapeAntiquoted t ++ "\""

escapeAntiquoted :: Antiquoted T.Text NExpr -> String
escapeAntiquoted a = case a of
    Plain t -> tail $ init $ show $ T.unpack t
    Antiquoted e -> "${" ++ exprI e ++ "}"

unOpStr :: NUnaryOp -> String
unOpStr op = case op of
    NNeg -> "-"
    NNot -> "!"

unaryOpI :: NUnaryOp -> NExpr -> String
unaryOpI op ex =
    unOpStr op ++ parenIf (unaryPrio op < exprPrio ex) (exprI ex)

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

binaryOpI :: NBinaryOp -> NExpr -> NExpr -> String
binaryOpI op l r =
    parenIf (
        binaryPrio op < exprPrio l || (
            binaryPrio op == exprPrio l &&
            not (associates (binOpOf l) op)
        )
    ) (exprI l) ++ " " ++
    binOpStr op ++ " " ++
    parenIf (
        binaryPrio op < exprPrio r || (
            binaryPrio op == exprPrio r &&
            not (associates op (binOpOf r))
        )
    ) (exprI r)

selectI :: NExpr -> NAttrPath NExpr -> Maybe NExpr -> String
selectI set attr def =
    parenIf (selectPrio <= exprPrio set) (exprI set) ++
    "." ++ pathI attr ++
    maybe ""
          (\x -> " or " ++ parenIf (selectPrio <= exprPrio x) (exprI x))
          def

hasAttrI :: NExpr -> NAttrPath NExpr -> String
hasAttrI set attr =
    parenIf (hasAttrPrio <= exprPrio set) (exprI set) ++
    " ? " ++ pathI attr

absI :: Params NExpr -> NExpr -> String
absI par ex = paramI par ++ ": " ++ exprI ex

paramI :: Params NExpr -> String
paramI par = case par of
    Param p -> T.unpack p
    ParamSet set Nothing -> paramSetI set
    ParamSet set (Just n) -> paramSetI set ++ " @ " ++ T.unpack n

paramSetI :: ParamSet NExpr -> String
paramSetI set = case set of
        FixedParamSet m -> "{ " ++ impl m ++ " }"
        VariadicParamSet m -> "{ " ++ impl m ++ ", ... }"
    where
        impl set =
            intercalate ", " (map (\(k, x) -> case x of
                Nothing -> T.unpack k
                Just e -> T.unpack k ++ " ? " ++ exprI e
            ) (M.toList set))

appI :: NExpr -> NExpr -> String
appI f x =
    parenIf (appPrio < exprPrio f) (exprI f) ++ " " ++ exprI x

setI :: Bool -> [Binding NExpr] -> String
setI rec binds =
    (if rec then "rec " else "") ++
    (if binds == [] then "{}"
     else "{ " ++ intercalate " " (map bindingI binds) ++ " }")

letI :: [Binding NExpr] -> NExpr -> String
letI binds ex =
    "let " ++ intercalate " " (map bindingI binds) ++ " in " ++
    exprI ex

ifI :: NExpr -> NExpr -> NExpr -> String
ifI cond then_ else_ =
    "if " ++ exprI cond ++ " then " ++ exprI then_ ++
    " else " ++ exprI else_;

stmtI :: String -> NExpr -> NExpr -> String
stmtI kw it expr = kw ++ " " ++ exprI it ++ "; " ++ exprI expr
