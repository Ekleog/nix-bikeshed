module Lib where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Zip

import Data.Fix
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T

import Debug.Trace

import Nix.Atoms
import Nix.Expr
import Nix.Parser

maxLineLength = 80

doTheThing :: String -> Bool -> IO ()
doTheThing file check = do
    expr <- parseFile file
    let indented = indentExpr maxLineLength expr;
    if check then do
        let check = parseStr indented;
        if expr `isEquivalentTo` check then
            putStrLn indented
        else
            error ("Unable to check the result: AST mismatch between\n" ++
                   "Source: " ++ show expr ++ "\n" ++
                   "Indented: " ++ show check ++ "\n")
    else
        putStrLn indented

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

isEquivalentTo :: NExpr -> NExpr -> Bool
isEquivalentTo a b = case (a, b) of
    (a, b) | a == b -> True
    (Fix (NStr a), Fix (NStr b)) | a `stringEquiv` b -> True
    (Fix (NList a), Fix (NList b)) | all (uncurry isEquivalentTo) (zip a b) -> True
    (Fix (NSet a), Fix (NSet b)) | all (uncurry bindingEquiv) (zip a b) -> True
    (Fix (NRecSet a), Fix (NRecSet b)) | all (uncurry bindingEquiv) (zip a b) -> True
    (Fix (NUnary x a), Fix (NUnary y b)) | x == y && a `isEquivalentTo` b -> True
    (Fix (NBinary x a b), Fix (NBinary y c d)) | x == y && a `isEquivalentTo` c
                                              && b `isEquivalentTo` d -> True
    (Fix (NSelect a b c), Fix (NSelect d e f)) | a `isEquivalentTo` d &&
                                                 b `pathEquiv` e &&
                                                 maybe (c == f) (uncurry isEquivalentTo) (mzip c f) -> True
    (Fix (NHasAttr a b), Fix (NHasAttr c d)) | a `isEquivalentTo` c &&
                                               b `pathEquiv` d -> True
    (Fix (NAbs x a), Fix (NAbs y b)) | x == y && a `isEquivalentTo` b -> True
    (Fix (NApp x a), Fix (NApp y b)) | x `isEquivalentTo` y &&
                                       a `isEquivalentTo` b -> True
    (Fix (NLet x a), Fix (NLet y b)) | all (uncurry bindingEquiv) (zip x y) &&
                                       a `isEquivalentTo` b -> True
    (Fix (NIf a b c), Fix (NIf d e f)) | a `isEquivalentTo` d &&
                                         b `isEquivalentTo` e &&
                                         c `isEquivalentTo` f -> True
    (Fix (NWith a b), Fix (NWith c d)) | a `isEquivalentTo` c &&
                                         b `isEquivalentTo` d -> True
    (Fix (NAssert a b), Fix (NAssert c d)) | a `isEquivalentTo` c &&
                                             b `isEquivalentTo` d -> True
    _ -> False

stringEquiv :: NString NExpr -> NString NExpr -> Bool
stringEquiv a b = all (uncurry antiquotEquiv)
                      (zip (stringContent a) (stringContent b))
    where stringContent a = case a of
            Indented x -> x
            DoubleQuoted x -> x

antiquotEquiv :: Antiquoted T.Text NExpr -> Antiquoted T.Text NExpr -> Bool
antiquotEquiv a b = case (a, b) of
    (a, b) | a == b -> True
    (Antiquoted a, Antiquoted b) | a `isEquivalentTo` b -> True
    _ -> False

bindingEquiv :: Binding NExpr -> Binding NExpr -> Bool
bindingEquiv a b = case (a, b) of
    (a, b) | a == b -> True
    (NamedVar a x, NamedVar b y) | a == b && x `isEquivalentTo` y -> True
    _ -> False

pathEquiv :: NAttrPath NExpr -> NAttrPath NExpr -> Bool
pathEquiv a b = all (uncurry keyNameEquiv) (zip a b)

keyNameEquiv :: NKeyName NExpr -> NKeyName NExpr -> Bool
keyNameEquiv a b = case (a, b) of
    (a, b) | a == b -> True

data WriteItem = WriteItem
    { isNewLine :: Bool
    , lineBegin :: String
    , text :: String }
type ColumnInfo = (Int, Int) -- (current column, maximal column)
type NixMonad a = WriterT [WriteItem] (State ColumnInfo) a

appendLine :: String -> NixMonad ()
appendLine x = do
    tell [WriteItem { isNewLine = False, lineBegin = "", text = x }]
    (col, max) <- get
    put $ (col + length x, max)

newLine :: NixMonad ()
newLine = do
    tell [WriteItem { isNewLine = True, lineBegin = "", text = "" }]
    (_, max) <- get
    put $ (0, max)

noop :: NixMonad ()
noop = tell []

indent :: NixMonad a -> NixMonad a
indent x = do
    (col, max) <- get
    put $ (col + 2, max)
    res <- flip censor x $ fmap $ \x -> x { lineBegin = "  " ++ lineBegin x }
    put $ (col, max)
    return res

intercalateM :: NixMonad () -> [NixMonad ()] -> NixMonad ()
intercalateM v l = sequence_ $ intersperse v l

indentExpr :: Int -> NExpr -> String
indentExpr maximalLineLength a =
    let nix = evalState (execWriterT (exprI a)) (0, maximalLineLength) in
    concatMap (\wi ->
        (if isNewLine wi then "\n" ++ lineBegin wi else "") ++ text wi
    ) nix

-- *I functions return the string that fits the constraints
exprI :: NExpr -> NixMonad ()
exprI expr = case expr of
    Fix (NConstant c) -> atomI c
    Fix (NStr s) -> stringI s
    Fix (NSym s) -> appendLine $ T.unpack s
    Fix (NList vals) -> listI vals
    Fix (NSet binds) -> setI False binds
    Fix (NRecSet binds) -> setI True binds
    Fix (NLiteralPath p) -> appendLine p
    Fix (NEnvPath p) -> appendLine $ "<" ++ p ++ ">"
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
    Fix (NList vals) -> listL vals
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

listPrio :: Int
listPrio = 2

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
    -- //
    (NUpdate, NUpdate) -> True

paren :: NixMonad () -> NixMonad ()
paren s = appendLine "(" >> s >> appendLine ")"

parenIf :: Bool -> NixMonad () -> NixMonad ()
parenIf cond = if cond then paren else id

bindingI :: Binding NExpr -> NixMonad ()
bindingI b = case b of
    NamedVar path val -> do
        pathI path >> appendLine " = " >> exprI val >> appendLine ";"
    Inherit set vars -> do
        appendLine "inherit "
        maybe noop (\s -> appendLine "(" >> exprI s >> appendLine ") ") set
        intercalateM (appendLine " ") (map keyNameI vars)
        appendLine ";"

bindingL :: Binding NExpr -> Int
bindingL b = case b of
    NamedVar path val -> 4 + pathL path + exprL val
    Inherit set vars ->
        8 + length vars +
        maybe 0 (\s -> 3 + exprL s) set + sum (map keyNameL vars)

listI :: [NExpr] -> NixMonad ()
listI vals = do
    appendLine "["
    intercalateM (appendLine " ")
                 (map (\e -> parenIf (listPrio <= exprPrio e) $ exprI e) vals)
    appendLine "]"

listL :: [NExpr] -> Int
listL vals =
    1 + length vals +
    sum (map (\e -> (if listPrio <= exprPrio e then 2 else 0) + exprL e) vals)

pathI :: NAttrPath NExpr -> NixMonad ()
pathI p = intercalateM (appendLine ".") $ map keyNameI p

pathL :: NAttrPath NExpr -> Int
pathL p = length p - 1 + sum (map keyNameL p)

keyNameI :: NKeyName NExpr -> NixMonad ()
keyNameI kn = case kn of
    StaticKey k -> appendLine $ T.unpack k
    DynamicKey (Plain s) -> stringI s
    DynamicKey (Antiquoted e) -> do
        appendLine "${"
        exprI e
        appendLine "}"

keyNameL :: NKeyName NExpr -> Int
keyNameL kn = case kn of
    StaticKey k -> T.length k
    DynamicKey (Plain s) -> stringL s
    DynamicKey (Antiquoted e) -> 3 + exprL e

atomI :: NAtom -> NixMonad ()
atomI a = case a of
    NInt i -> appendLine $ show i
    NBool True -> appendLine $ "true"
    NBool False -> appendLine $ "false"
    NNull -> appendLine $ "null"
    NUri t -> appendLine $ T.unpack t

atomL :: NAtom -> Int
atomL a = case a of
    NInt i -> length $ show i
    NBool True -> 4
    NBool False -> 5
    NNull -> 4
    NUri t -> T.length t

stringI :: NString NExpr -> NixMonad ()
stringI s = case s of
    DoubleQuoted t -> stringI (Indented t) -- ignore string type
    Indented t -> do
        (col, max) <- get
        if col + stringL (Indented t) <= max then do
            appendLine "\""
            mapM_ escapeAntiquotedI t
            appendLine "\""
        else do
            appendLine "''"
            lastEndedWithNewLine <- indent $ do
                newLine
                foldM
                    (\lastEndedWithNewLine -> \item -> do
                        let (endsWithNewLine, output) = escapeMultilineI item;
                        when lastEndedWithNewLine newLine
                        output
                        return endsWithNewLine)
                    False
                    t
            when lastEndedWithNewLine newLine
            appendLine "''"

stringL :: NString NExpr -> Int
stringL s = case s of
    DoubleQuoted t -> stringL (Indented t) -- ignore string type
    Indented t -> 2 + sum (map escapeAntiquotedL t)

escapeAntiquotedI :: Antiquoted T.Text NExpr -> NixMonad ()
escapeAntiquotedI a = case a of
    Plain t -> appendLine $ tail $ init $ show $ T.unpack t
    Antiquoted e -> do
        appendLine "${"
        exprI e
        appendLine "}"

-- returns (endsWithNewLine, output)
escapeMultilineI :: Antiquoted T.Text NExpr -> (Bool, NixMonad ())
escapeMultilineI a = case a of
    Plain t -> (last (T.unpack t) == '\n',
                intercalateM newLine $ map appendLine $ lines $ T.unpack t)
    Antiquoted e -> (False, appendLine "${" >> exprI e >> appendLine "}")

escapeAntiquotedL :: Antiquoted T.Text NExpr -> Int
escapeAntiquotedL a = case a of
    Plain t -> length (show $ T.unpack t) - 2
    Antiquoted e -> 3 + exprL e

unOpStr :: NUnaryOp -> String
unOpStr op = case op of
    NNeg -> "-"
    NNot -> "!"

unaryOpI :: NUnaryOp -> NExpr -> NixMonad ()
unaryOpI op ex = do
    appendLine $ unOpStr op
    parenIf (unaryPrio op < exprPrio ex) (exprI ex)

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

binaryOpI :: NBinaryOp -> NExpr -> NExpr -> NixMonad ()
binaryOpI op l r = do
    parenIf (binaryOpNeedsParen True op l) (exprI l)
    appendLine $ " " ++ binOpStr op ++ " "
    parenIf (binaryOpNeedsParen False op r) (exprI r)

binaryOpL :: NBinaryOp -> NExpr -> NExpr -> Int
binaryOpL op l r =
    exprL l + exprL r + length (binOpStr op) + 2 +
    (if binaryOpNeedsParen True op l then 2 else 0) +
    (if binaryOpNeedsParen False op r then 2 else 0)

selectI :: NExpr -> NAttrPath NExpr -> Maybe NExpr -> NixMonad ()
selectI set attr def = do
    parenIf (selectPrio <= exprPrio set) (exprI set)
    appendLine "."
    pathI attr
    maybe noop
          (\x -> do
            appendLine " or "
            parenIf (selectPrio <= exprPrio x) (exprI x))
          def

selectL :: NExpr -> NAttrPath NExpr -> Maybe NExpr -> Int
selectL set attr def =
    1 + (if selectPrio <= exprPrio set then 2 else 0) + exprL set + pathL attr +
    maybe 0
          (\x -> 4 + (if (selectPrio <= exprPrio x) then 2 else 0) + exprL x)
          def

hasAttrI :: NExpr -> NAttrPath NExpr -> NixMonad ()
hasAttrI set attr = do
    parenIf (hasAttrPrio <= exprPrio set) (exprI set)
    appendLine " ? "
    pathI attr

hasAttrL :: NExpr -> NAttrPath NExpr -> Int
hasAttrL set attr =
    3 + (if hasAttrPrio < exprPrio set then 2 else 0) + exprL set + pathL attr

absI :: Params NExpr -> NExpr -> NixMonad ()
absI par ex = do
    (col, max) <- get
    if col + absL par ex <= max then
        paramI par >> appendLine ": " >> exprI ex
    else
        paramI par >> appendLine ":" >> newLine >> exprI ex

absL :: Params NExpr -> NExpr -> Int
absL par ex = paramL par + 2 + exprL ex

paramI :: Params NExpr -> NixMonad ()
paramI par = case par of
    Param p -> appendLine $ T.unpack p
    ParamSet set name -> do
        paramSetI set
        maybe noop (\n -> appendLine " @ " >> appendLine (T.unpack n)) name

paramL :: Params NExpr -> Int
paramL par = case par of
    Param p -> T.length p
    ParamSet set name -> paramSetL set + maybe 0 (\n -> 3 + T.length n) name

paramSetI :: ParamSet NExpr -> NixMonad ()
paramSetI set = case set of
        FixedParamSet m -> do
            (col, max) <- get
            if col + paramSetL set <= max then do
                appendLine "{ "
                paramSetContentsI False (M.toList m)
                appendLine " }"
            else do
                appendLine "{"
                indent $ newLine >> paramSetContentsI True (M.toList m)
                newLine >> appendLine "}"
        VariadicParamSet m -> do
            (col, max) <- get
            if col + paramSetL set <= max then do
                appendLine "{ "
                paramSetContentsI False (M.toList m)
                appendLine ", ... }"
            else do
                appendLine "{"
                indent $ do
                    newLine
                    paramSetContentsI True (M.toList m) >> appendLine ","
                    newLine >> appendLine "..."
                newLine >> appendLine "}"

paramSetContentsI :: Bool -> [(T.Text, Maybe NExpr)] -> NixMonad ()
paramSetContentsI intersperseLines set =
    intercalateM
        (if intersperseLines then appendLine "," >> newLine
         else appendLine ", ")
        (map (\(k, x) -> do
            appendLine $ T.unpack k
            maybe noop (\e -> appendLine " ? " >> exprI e) x
         ) set)

paramSetL :: ParamSet NExpr -> Int
paramSetL set = case set of
    FixedParamSet m -> 4 + paramSetContentsL (M.toList m)
    VariadicParamSet m -> 9 + paramSetContentsL (M.toList m)

paramSetContentsL :: [(T.Text, Maybe NExpr)] -> Int
paramSetContentsL l =
    2 * (length l - 1) +
    sum (map (\(k, x) -> T.length k + maybe 0 (\e -> 3 + exprL e) x) l)

appI :: NExpr -> NExpr -> NixMonad ()
appI f x = do
    parenIf (appPrio < exprPrio f) (exprI f)
    appendLine " "
    parenIf (appPrio <= exprPrio x) (exprI x)

appL :: NExpr -> NExpr -> Int
appL f x =
    (if appPrio < exprPrio f then 2 else 0) + exprL f + 1 +
    (if appPrio <= exprPrio x then 2 else 0) + exprL x

setI :: Bool -> [Binding NExpr] -> NixMonad ()
setI rec binds = do
    when rec $ appendLine "rec "
    if binds == [] then appendLine "{}"
    else do
        (col, max) <- get
        if col + setL False binds <= max then do
            appendLine "{ "
            intercalateM (appendLine " ") $ map bindingI binds
            appendLine " }"
        else do
            appendLine "{"
            indent $ newLine >> intercalateM newLine (map bindingI binds)
            newLine >> appendLine "}"

setL :: Bool -> [Binding NExpr] -> Int
setL rec binds =
    (if rec then 4 else 0) +
    (if binds == [] then 2 else 3 + length binds + sum (map bindingL binds))

letI :: [Binding NExpr] -> NExpr -> NixMonad ()
letI binds ex = do
    (col, max) <- get
    if col + letL binds ex <= max then do
        appendLine "let "
        intercalateM (appendLine " ") (map bindingI binds)
        appendLine " in "
        exprI ex
    else do
        appendLine "let"
        indent $ newLine >> intercalateM newLine (map bindingI binds)
        newLine >> appendLine "in" >> newLine
        exprI ex

letL :: [Binding NExpr] -> NExpr -> Int
letL binds ex = 7 + length binds + exprL ex + sum (map bindingL binds)

ifI :: NExpr -> NExpr -> NExpr -> NixMonad ()
ifI cond then_ else_ = do
    appendLine "if "
    exprI cond
    appendLine " then "
    exprI then_
    appendLine " else "
    exprI else_;

ifL :: NExpr -> NExpr -> NExpr -> Int
ifL c t e = 15 + exprL c + exprL t + exprL e

stmtI :: String -> NExpr -> NExpr -> NixMonad ()
stmtI kw it expr = do
    appendLine $ kw ++ " "
    exprI it
    appendLine "; "
    exprI expr

stmtL :: Int -> NExpr -> NExpr -> Int
stmtL kwlen it expr = kwlen + 3 + exprL it + exprL expr
