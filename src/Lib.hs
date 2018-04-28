{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, RankNTypes #-}
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

newtype WrapNExpr = WrapNExpr NExpr
instance Eq WrapNExpr where
    (WrapNExpr a) == (WrapNExpr b) = a `isEquivalentTo` b
isEquivalentTo :: NExpr -> NExpr -> Bool
isEquivalentTo (Fix a) (Fix b) = case (fmap WrapNExpr a, fmap WrapNExpr b) of
    (NStr a, NStr b) -> stringContent a == stringContent b
        where
            stringContent a = case a of
                Indented x -> x
                DoubleQuoted x -> x
    (a, b) -> a == b


class Monad m => IndentMonad m where
    appendLine :: String -> m ()
    newLine :: m ()
    indent :: m a -> m a
    hasSpaceFor :: Int -> m Bool

-- If there is enough space on the line, run (x True), otherwise run (x False)
-- In CountMonad, this simply runs (x True)
tryOneLine :: forall m. IndentMonad m => (forall n. IndentMonad n => Bool -> n ()) -> m ()
tryOneLine x = hasSpaceFor (runCountMonad (x True)) >>= x

data WriteItem = WriteItem
    { isNewLine :: Bool
    , lineBegin :: String
    , text :: String }
type ColumnInfo = (Int, Int) -- (current column, maximal column)

type NixMonad = WriterT [WriteItem] (State ColumnInfo)

runNixMonad :: Int -> NixMonad () -> [WriteItem]
runNixMonad maximalLineLength m = evalState (execWriterT m) (0, maximalLineLength)

instance IndentMonad NixMonad where
    appendLine x = do
        tell [WriteItem { isNewLine = False, lineBegin = "", text = x }]
        (col, max) <- get
        put $ (col + length x, max)

    newLine = do
        tell [WriteItem { isNewLine = True, lineBegin = "", text = "" }]
        (_, max) <- get
        put $ (0, max)

    indent x = do
        (col, max) <- get
        put $ (col + 2, max)
        res <- flip censor x $ fmap $ \x -> x { lineBegin = "  " ++ lineBegin x }
        put $ (col, max)
        return res

    hasSpaceFor x = do
        (col, max) <- get
        return (col + x <= max)


type CountMonad = Writer (Sum Int)

runCountMonad :: CountMonad () -> Int
runCountMonad = getSum . execWriter

instance IndentMonad CountMonad where
    appendLine x = tell (Sum $ length x)
    newLine = return ()
    indent x = x
    hasSpaceFor x = return True

noop :: Monad m => m ()
noop = return ()

intercalateM :: Monad m => m () -> [m ()] -> m ()
intercalateM v l = sequence_ $ intersperse v l

indentExpr :: Int -> NExpr -> String
indentExpr maximalLineLength a =
    let nix = runNixMonad maximalLineLength (exprI a) in
    concatMap (\wi ->
        (if isNewLine wi then "\n" ++ lineBegin wi else "") ++ text wi
    ) nix

-- *I functions return the string that fits the constraints
exprI :: IndentMonad m => NExpr -> m ()
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
exprL = runCountMonad . exprI

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

paren :: IndentMonad m => m () -> m ()
paren s = appendLine "(" >> s >> appendLine ")"

parenIf :: IndentMonad m => Bool -> m () -> m ()
parenIf cond = if cond then paren else id

bindingI :: IndentMonad m => Binding NExpr -> m ()
bindingI b = case b of
    NamedVar path val -> do
        pathI path >> appendLine " = " >> exprI val >> appendLine ";"
    Inherit set vars -> do
        appendLine "inherit "
        maybe noop (\s -> appendLine "(" >> exprI s >> appendLine ") ") set
        intercalateM (appendLine " ") (map keyNameI vars)
        appendLine ";"

listI :: IndentMonad m => [NExpr] -> m ()
listI vals = do
    appendLine "["
    intercalateM (appendLine " ")
                 (map (\e -> parenIf (listPrio <= exprPrio e) $ exprI e) vals)
    appendLine "]"

pathI :: IndentMonad m => NAttrPath NExpr -> m ()
pathI p = intercalateM (appendLine ".") $ map keyNameI p

keyNameI :: IndentMonad m => NKeyName NExpr -> m ()
keyNameI kn = case kn of
    StaticKey k -> appendLine $ T.unpack k
    DynamicKey (Plain s) -> stringI s
    DynamicKey (Antiquoted e) -> do
        appendLine "${"
        exprI e
        appendLine "}"

atomI :: IndentMonad m => NAtom -> m ()
atomI a = case a of
    NInt i -> appendLine $ show i
    NBool True -> appendLine $ "true"
    NBool False -> appendLine $ "false"
    NNull -> appendLine $ "null"
    NUri t -> appendLine $ T.unpack t

stringI :: IndentMonad m => NString NExpr -> m ()
stringI s = let t = case s of
                    { DoubleQuoted t -> t; Indented t -> t }
    in tryOneLine (\b ->
        if b then do
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
            appendLine "''")

escapeAntiquotedI :: IndentMonad m => Antiquoted T.Text NExpr -> m ()
escapeAntiquotedI a = case a of
    Plain t -> appendLine $ tail $ init $ show $ T.unpack t
    Antiquoted e -> do
        appendLine "${"
        exprI e
        appendLine "}"

-- returns (endsWithNewLine, output)
escapeMultilineI :: IndentMonad m => Antiquoted T.Text NExpr -> (Bool, m ())
escapeMultilineI a = case a of
    Plain t -> (last (T.unpack t) == '\n',
                intercalateM newLine $ map appendLine $ lines $ T.unpack t)
    Antiquoted e -> (False, appendLine "${" >> exprI e >> appendLine "}")

unOpStr :: NUnaryOp -> String
unOpStr op = case op of
    NNeg -> "-"
    NNot -> "!"

unaryOpI :: IndentMonad m => NUnaryOp -> NExpr -> m ()
unaryOpI op ex = do
    appendLine $ unOpStr op
    parenIf (unaryPrio op < exprPrio ex) (exprI ex)

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

binaryOpI :: IndentMonad m => NBinaryOp -> NExpr -> NExpr -> m ()
binaryOpI op l r = do
    parenIf (binaryOpNeedsParen True op l) (exprI l)
    appendLine $ " " ++ binOpStr op ++ " "
    parenIf (binaryOpNeedsParen False op r) (exprI r)

selectI :: IndentMonad m => NExpr -> NAttrPath NExpr -> Maybe NExpr -> m ()
selectI set attr def = do
    parenIf (selectPrio <= exprPrio set) (exprI set)
    appendLine "."
    pathI attr
    maybe noop
          (\x -> do
            appendLine " or "
            parenIf (selectPrio <= exprPrio x) (exprI x))
          def

hasAttrI :: IndentMonad m => NExpr -> NAttrPath NExpr -> m ()
hasAttrI set attr = do
    parenIf (hasAttrPrio <= exprPrio set) (exprI set)
    appendLine " ? "
    pathI attr

absI :: IndentMonad m => Params NExpr -> NExpr -> m ()
absI par ex = paramI par >> appendLine ": " >> exprI ex

paramI :: IndentMonad m => Params NExpr -> m ()
paramI par = case par of
    Param p -> appendLine $ T.unpack p
    ParamSet set name -> do
        paramSetI set
        maybe noop (\n -> appendLine " @ " >> appendLine (T.unpack n)) name

paramSetI :: IndentMonad m => ParamSet NExpr -> m ()
paramSetI set = tryOneLine
    (\oneLine -> do
        let space = if oneLine then appendLine " " else newLine
        appendLine "{"
        indent $ space
        indent $ case set of
            FixedParamSet m -> do
                paramSetContentsI space (M.toList m)
            VariadicParamSet m -> do
                paramSetContentsI space (M.toList m)
                appendLine ","
                space >> appendLine "..."
        space >> appendLine "}")

paramSetContentsI :: IndentMonad m => m () -> [(T.Text, Maybe NExpr)] -> m ()
paramSetContentsI space set =
    intercalateM
        (appendLine "," >> space)
        (map (\(k, x) -> do
            appendLine $ T.unpack k
            maybe noop (\e -> appendLine " ? " >> exprI e) x
         ) set)

appI :: IndentMonad m => NExpr -> NExpr -> m ()
appI f x = do
    parenIf (appPrio < exprPrio f) (exprI f)
    appendLine " "
    parenIf (appPrio <= exprPrio x) (exprI x)

setI :: IndentMonad m => Bool -> [Binding NExpr] -> m ()
setI rec binds = do
    when rec $ appendLine "rec "
    if binds == [] then appendLine "{}"
    else tryOneLine
        (\oneLine -> do
            let space = if oneLine then appendLine " " else newLine
            appendLine "{"
            indent $ space >> intercalateM space (map bindingI binds)
            space >> appendLine "}")

letI :: IndentMonad m => [Binding NExpr] -> NExpr -> m ()
letI binds ex = tryOneLine
    (\oneLine -> do
        let space = if oneLine then appendLine " " else newLine
        appendLine "let"
        indent $ space >> intercalateM space (map bindingI binds)
        space >> appendLine "in" >> space
        exprI ex)

ifI :: IndentMonad m => NExpr -> NExpr -> NExpr -> m ()
ifI cond then_ else_ = do
    appendLine "if "
    exprI cond
    appendLine " then "
    exprI then_
    appendLine " else "
    exprI else_;

stmtI :: IndentMonad m => String -> NExpr -> NExpr -> m ()
stmtI kw it expr = do
    appendLine $ kw ++ " "
    exprI it
    appendLine "; "
    exprI expr
