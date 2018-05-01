{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings, LambdaCase #-}
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
import Nix.Pretty
import qualified Nix.Parser.Operators as Nix
import qualified Nix.StringOperations as Nix

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


data WriteItem =
      NewLineItem
    | TextItem T.Text
    | BlockItem [WriteItem]
    | IndentedItem WriteItem
    | BreakableItem Int WriteItem
    | QuotedItem WriteItem
    | AntiQuotedItem WriteItem
    | BreakableSpaceItem
    deriving (Eq)

instance Monoid WriteItem where
    mempty = BlockItem []
    BlockItem x `mappend` BlockItem y = BlockItem (x ++ y)
    BlockItem x `mappend` item' = BlockItem (x ++ [item'])
    item `mappend` BlockItem y = BlockItem ([item] ++ y)
    item `mappend` item' = BlockItem ([item, item'])

flattenLineTree :: Int -> WriteItem -> T.Text
flattenLineTree maximalLineLength = execWriter . aux 0 True False
    where
        indentSize = 2
        newline idt = tell $ "\n" <> T.replicate idt " "
        aux :: Int -> Bool -> Bool -> WriteItem -> Writer T.Text ()
        aux idt break quoted = \case
            NewLineItem | quoted && not break ->
                tell "\\n"
            NewLineItem ->
                newline idt
            TextItem str | quoted && not break ->
                tell . T.pack . tail . init . show $ str
            TextItem str ->
                tell str
            BlockItem ws ->
                mapM_ (aux idt break quoted) ws
            IndentedItem item ->
                aux (idt + indentSize) break quoted item
            BreakableItem len item ->
                aux idt (idt + len > maximalLineLength) quoted item
            QuotedItem item -> do
                tell $ if break then "''" else "\""
                when break $ aux idt break False $ IndentedItem NewLineItem
                aux idt break True item
                tell $ if break then "''" else "\""
            AntiQuotedItem item ->
                aux idt break False item
            BreakableSpaceItem | break ->
                newline idt
            BreakableSpaceItem ->
                tell " "


type NixMonad = Writer (Sum Int, WriteItem)

runNixMonad :: NixMonad () -> WriteItem
runNixMonad = snd . execWriter

runCount :: NixMonad () -> Int
runCount = getSum . fst . execWriter

appendLine :: T.Text -> NixMonad ()
appendLine x = tell (Sum $ T.length x, TextItem x)

appendQuotedLine :: T.Text -> NixMonad ()
appendQuotedLine x = tell (Sum $ length (show x) - 2, TextItem x)

newLine :: NixMonad ()
newLine = tell (Sum 0, NewLineItem)

quotedNewLine :: NixMonad ()
quotedNewLine = tell (Sum 2, NewLineItem)

indent :: NixMonad a -> NixMonad a
indent = censor $ \(n, ws) -> (n, IndentedItem ws)

tryOneLine :: NixMonad () -> NixMonad ()
tryOneLine = censor $ \(n, ws) -> (n, BreakableItem (getSum n) ws)

breakableSpace :: NixMonad ()
breakableSpace = tell (Sum 1, BreakableSpaceItem)

antiquote :: NixMonad () -> NixMonad ()
antiquote = censor (\(n, ws) -> (n, AntiQuotedItem ws))

quote :: NixMonad () -> NixMonad ()
quote x =
    censor (\(n, ws) -> (n, QuotedItem ws)) $ do
        tell (Sum 2, mempty) -- quotes
        x


noop :: Monad m => m ()
noop = return ()

intercalateM :: Monad m => m () -> [m ()] -> m ()
intercalateM v l = sequence_ $ intersperse v l


indentExpr :: Int -> NExpr -> String
indentExpr maximalLineLength a =
    let lineTree = runNixMonad (exprI a) in
        T.unpack $ flattenLineTree maximalLineLength lineTree

-- *I functions return the string that fits the constraints
exprI :: NExpr -> NixMonad ()
exprI (Fix expr) = case expr of
    NConstant c -> atomI c
    NStr s -> stringI s
    NSym s -> appendLine s
    NList vals -> listI vals
    NSet binds -> setI False binds
    NRecSet binds -> setI True binds
    NLiteralPath p -> appendLine $ T.pack p
    NEnvPath p -> appendLine $ T.pack $ "<" ++ p ++ ">"
    NUnary op ex -> unaryOpI op ex
    NBinary op l r -> binaryOpI op l r
    NSelect set attr def -> selectI set attr def
    NHasAttr set attr -> hasAttrI set attr
    NAbs param expr -> absI param expr
    NApp f x -> appI f x
    NLet binds ex -> letI binds ex
    NIf cond then_ else_ -> ifI cond then_ else_
    NWith set expr -> stmtI "with" set expr
    NAssert test expr -> stmtI "assert" test expr

-- *L functions return the length the expression would take if put all on one
-- line
exprL :: NExpr -> Int
exprL = runCount . exprI


data Prio
    = NoPrio
    | HNixPrio Nix.OperatorInfo
    | MaxPrio

instance Eq Prio where
    NoPrio == NoPrio = True
    MaxPrio == MaxPrio = True
    HNixPrio x == HNixPrio y =
        Nix.precedence x == Nix.precedence y
    _ == _ = False

instance Ord Prio where
    NoPrio `compare` NoPrio = EQ
    NoPrio `compare` _ = LT
    MaxPrio `compare` MaxPrio = EQ
    MaxPrio `compare` _ = GT
    _ `compare` NoPrio = GT
    _ `compare` MaxPrio = LT
    HNixPrio x `compare` HNixPrio y =
        Nix.precedence y `compare` Nix.precedence x

binOpOf :: Prio -> String
binOpOf (HNixPrio op) = Nix.operatorName op

binOpStr :: NBinaryOp -> T.Text
binOpStr = T.pack . Nix.operatorName . Nix.getBinaryOperator

unOpStr :: NUnaryOp -> T.Text
unOpStr = T.pack . Nix.operatorName . Nix.getUnaryOperator

-- Less binds stronger
exprPrio :: NExpr -> Prio
exprPrio (Fix expr) = case expr of
    -- See https://nixos.org/nix/manual/#table-operators
    NSelect _ _ _ -> selectPrio
    NApp _ _ -> appPrio
    NUnary op _ -> unaryPrio op
    NHasAttr _ _ -> hasAttrPrio
    NBinary op _ _ -> binaryPrio op
    NAbs _ _ -> MaxPrio
    NLet _ _ -> MaxPrio
    NIf _ _ _ -> MaxPrio
    NWith _ _ -> MaxPrio
    NAssert _ _ -> MaxPrio
    _ -> NoPrio

listPrio, appPrio, selectPrio, hasAttrPrio :: Prio
listPrio = HNixPrio Nix.appOp
appPrio = HNixPrio Nix.appOp
selectPrio = HNixPrio Nix.selectOp
hasAttrPrio = HNixPrio Nix.hasAttrOp

unaryPrio :: NUnaryOp -> Prio
unaryPrio = HNixPrio . Nix.getUnaryOperator
binaryPrio :: NBinaryOp -> Prio
binaryPrio = HNixPrio . Nix.getBinaryOperator


associates :: Prio -> Prio -> Bool
associates l r = case (binOpOf l, binOpOf r) of
    -- *, /
    ("*", "*") -> True
    ("*", "/") -> True
    ("/", "*") -> False
    ("/", "/") -> False
    -- +, -
    ("+", "+") -> True
    ("+", "-") -> True
    ("-", "+") -> False
    ("-", "-") -> False
    -- ==, !=
    ("==", "==") -> False
    ("==", "!=") -> False
    ("!=", "==") -> False
    ("!=", "!=") -> False
    -- //
    ("//", "//") -> True

paren :: NixMonad () -> NixMonad ()
paren s = appendLine "(" >> s >> appendLine ")"

parenIf :: Bool -> NixMonad () -> NixMonad ()
parenIf cond = if cond then paren else id

parenExprI :: (Prio -> Bool) -> NExpr -> NixMonad ()
parenExprI pred e = parenIf (pred $ exprPrio e) (exprI e)

binaryOpNeedsParen :: Bool -> Prio -> Prio -> Bool
binaryOpNeedsParen isLeftChild opprio childprio =
    opprio < childprio || (
        opprio == childprio &&
        not ((if isLeftChild then id else flip)
            associates childprio opprio)
    )

bindingI :: Binding NExpr -> NixMonad ()
bindingI b = case b of
    NamedVar path val -> do
        pathI path >> appendLine " = " >> exprI val >> appendLine ";"
    Inherit set vars -> do
        appendLine "inherit "
        maybe noop (\s -> appendLine "(" >> exprI s >> appendLine ") ") set
        intercalateM (appendLine " ") (map keyNameI vars)
        appendLine ";"

listI :: [NExpr] -> NixMonad ()
listI vals = do
    appendLine "["
    intercalateM (appendLine " ")
                 (map (parenExprI (listPrio <=)) vals)
    appendLine "]"

pathI :: NAttrPath NExpr -> NixMonad ()
pathI p = intercalateM (appendLine ".") $ map keyNameI p

keyNameI :: NKeyName NExpr -> NixMonad ()
keyNameI kn = case kn of
    StaticKey k -> appendLine k
    DynamicKey (Plain s) -> stringI s
    DynamicKey (Antiquoted e) -> do
        appendLine "${"
        exprI e
        appendLine "}"

atomI :: NAtom -> NixMonad ()
atomI = appendLine . atomText

stringI :: NString NExpr -> NixMonad ()
stringI s = let
        doLine :: [Antiquoted T.Text NExpr] -> NixMonad ()
        doLine = mapM_ $ \case
                    Plain t -> appendQuotedLine t
                    Antiquoted e -> do
                        appendLine "${"
                        antiquote $ exprI e
                        appendLine "}"

        t = case s of { DoubleQuoted t -> t; Indented t -> t }
        lines = Nix.splitLines t
        (lines', endsWithNewLine) =
            if (last lines == [Plain $ T.pack ""]) -- if ends with newline
               then (init lines, True)
               else (lines, False)

    in tryOneLine $ quote $ do
        indent $
            intercalateM quotedNewLine $
                map doLine lines'
        when endsWithNewLine newLine

unaryOpI :: NUnaryOp -> NExpr -> NixMonad ()
unaryOpI op ex = do
    appendLine $ unOpStr op
    parenExprI (unaryPrio op <) ex


binaryOpI :: NBinaryOp -> NExpr -> NExpr -> NixMonad ()
binaryOpI op l r = do
    parenExprI (binaryOpNeedsParen True (binaryPrio op)) l
    appendLine $ " " <> binOpStr op <> " "
    parenExprI (binaryOpNeedsParen False (binaryPrio op)) r

selectI :: NExpr -> NAttrPath NExpr -> Maybe NExpr -> NixMonad ()
selectI set attr def = do
    parenExprI (selectPrio <=) set
    appendLine "."
    pathI attr
    maybe noop
          (\x -> do
            appendLine " or "
            parenExprI (selectPrio <=) x)
          def

hasAttrI :: NExpr -> NAttrPath NExpr -> NixMonad ()
hasAttrI set attr = do
    parenExprI (hasAttrPrio <=) set
    appendLine " ? "
    pathI attr

absI :: Params NExpr -> NExpr -> NixMonad ()
absI par ex = paramI par >> appendLine ": " >> exprI ex

paramI :: Params NExpr -> NixMonad ()
paramI par = case par of
    Param p -> appendLine p
    ParamSet set name -> do
        paramSetI set
        maybe noop (\n -> appendLine " @ " >> appendLine n) name

paramSetI :: ParamSet NExpr -> NixMonad ()
paramSetI set = tryOneLine $ do
        appendLine "{"
        indent breakableSpace
        indent $ do
            let (m, isVariadic) = case set of
                        { FixedParamSet m -> (m, False); VariadicParamSet m -> (m, True) }
            intercalateM
                (appendLine "," >> breakableSpace)
                (map (\(k, x) -> do
                    appendLine k
                    maybe noop (\e -> appendLine " ? " >> exprI e) x
                 ) (M.toList m))
            when isVariadic $ appendLine "," >> breakableSpace >> appendLine "..."
        breakableSpace >> appendLine "}"

appI :: NExpr -> NExpr -> NixMonad ()
appI f x = do
    parenExprI (appPrio <) f
    appendLine " "
    parenExprI (appPrio <=) x

setI :: Bool -> [Binding NExpr] -> NixMonad ()
setI rec binds = do
    when rec $ appendLine "rec "
    if binds == [] then appendLine "{}"
    else tryOneLine $ do
            appendLine "{"
            indent $ breakableSpace >> intercalateM breakableSpace (map bindingI binds)
            breakableSpace >> appendLine "}"

letI :: [Binding NExpr] -> NExpr -> NixMonad ()
letI binds ex = tryOneLine $ do
        appendLine "let"
        indent $ breakableSpace >> intercalateM breakableSpace (map bindingI binds)
        breakableSpace >> appendLine "in" >> breakableSpace
        exprI ex

ifI :: NExpr -> NExpr -> NExpr -> NixMonad ()
ifI cond then_ else_ = do
    appendLine "if "
    exprI cond
    appendLine " then "
    exprI then_
    appendLine " else "
    exprI else_

stmtI :: T.Text -> NExpr -> NExpr -> NixMonad ()
stmtI kw it expr = do
    appendLine $ kw <> " "
    exprI it
    appendLine "; "
    exprI expr
