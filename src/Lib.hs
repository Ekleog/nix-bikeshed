{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, OverloadedStrings, LambdaCase #-}
module Lib where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Reader
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

indentSize, maxLineLength :: Indent
maxLineLength = 80
indentSize = 2

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


noop :: Monad m => m ()
noop = return ()

intercalateM :: Monad m => m () -> [m ()] -> m ()
intercalateM v l = sequence_ $ intersperse v l


indentExpr :: Int -> NExpr -> String
indentExpr maximalLineLength a = T.unpack $
    let lineTree = runNixMonad (exprI a) in
    runIndentTreeMonad $ flattenLineTree maximalLineLength lineTree



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

-- state: if last output was a newline, we need to write the indent
-- reader: (idt, break, escape), where `idt` is the current indent, `break`
--   indicates if the current block was deemed too long to fit on a single
--   line and must be broken, and `escape` indicates if we are currently writing
--   the contents of a double quoted string and need to escape things
type NeedWriteIndent = Bool
type Indent = Int
type Break = Bool
type Escape = Bool
type IndentTreeMonad = StateT NeedWriteIndent (ReaderT (Indent, Break, Escape) (Writer T.Text))

runIndentTreeMonad :: IndentTreeMonad () -> T.Text
runIndentTreeMonad = execWriter .
            flip runReaderT (0, True, False) .
            flip evalStateT True

localIdt :: Indent -> IndentTreeMonad a -> IndentTreeMonad a
localIdt idt = local (\(_, break, escape) -> (idt, break, escape))
localBreak :: Break -> IndentTreeMonad a -> IndentTreeMonad a
localBreak break = local (\(idt, _, escape) -> (idt, break, escape))
localEscape :: Escape -> IndentTreeMonad a -> IndentTreeMonad a
localEscape escape = local (\(idt, break, _) -> (idt, break, escape))

escapeStr :: T.Text -> T.Text
escapeStr = T.pack . tail . init . show

flattenLineTree :: Int -> WriteItem -> IndentTreeMonad ()
flattenLineTree maximalLineLength = \case
    NewLineItem -> do
        (_, _, escape) <- ask
        if escape
           then write "\\n"
           else newline
    TextItem str -> do
        (_, _, escape) <- ask
        write $ if escape
           then escapeStr $ str
           else str
    BlockItem ws ->
        forM_ ws aux
    IndentedItem item -> do
        (idt, _, _) <- ask
        localIdt (idt+indentSize) $ aux item
    BreakableItem len item -> do
        (idt, _, _) <- ask
        localBreak (idt + len > maximalLineLength) $ aux item
    QuotedItem item -> do
        (_, break, _) <- ask
        write $ if break then "''" else "\""
        localEscape (not break) $ aux $
            (if break then IndentedItem NewLineItem else mempty)
            <> item
        write $ if break then "''" else "\""
    AntiQuotedItem item ->
        localEscape False $ aux item
    BreakableSpaceItem -> do
        (_, break, _) <- ask
        if break
           then newline
           else write " "
    where
        aux = flattenLineTree maximalLineLength
        newline = tell "\n" >> put True
        write str | T.null str = return ()
        write str = do
            needWriteIndent <- get
            when needWriteIndent $ do
                (idt, _, _) <- ask
                tell $ T.replicate idt " "
                put False
            tell str



-- Writes the length of the output (if it was on one line and with double quoted strings)
-- along with a tree representation of the output
type NixMonad = Writer (Sum Int, WriteItem)

runNixMonad :: NixMonad () -> WriteItem
runNixMonad = snd . execWriter

runCount :: NixMonad () -> Int
runCount = getSum . fst . execWriter

writeText :: T.Text -> NixMonad ()
writeText x = tell (Sum $ T.length x, TextItem x)

writeQuotedText :: T.Text -> NixMonad ()
writeQuotedText x = tell (Sum $ length (show x) - 2, TextItem x)

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


-- *I functions return the string that fits the constraints
exprI :: NExpr -> NixMonad ()
exprI (Fix expr) = formatI $ parenthesizeI expr

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
exprPrio :: NExprF a -> Prio
exprPrio = \case
    -- See https://nixos.org/nix/manual/#table-operators
    NSelect _ _ _ -> selectPrio
    NApp _ _ -> appPrio
    NList _ -> listPrio
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
paren s = writeText "(" >> s >> writeText ")"

parenIf :: Bool -> NixMonad () -> NixMonad ()
parenIf cond = if cond then paren else id

parenExprI :: (Prio -> Bool) -> NExpr -> NixMonad ()
parenExprI pred e = parenIf (pred $ exprPrio $ unFix e) (exprI e)

binaryOpNeedsParen :: Bool -> Prio -> Prio -> Bool
binaryOpNeedsParen isLeftChild opprio childprio =
    opprio < childprio || (
        opprio == childprio &&
        not ((if isLeftChild then id else flip)
            associates childprio opprio)
    )

parenthesizeI :: NExprF NExpr -> NExprF (NixMonad ())
parenthesizeI expr =
    let prio = exprPrio expr in
    case expr of
        NBinary op l r -> NBinary op
            (parenExprI (binaryOpNeedsParen True prio) l)
            (parenExprI (binaryOpNeedsParen False prio) r)
        NApp f x -> NApp (parenExprI (prio <) f) (parenExprI (prio <=) x)
        NUnary op _ -> fmap (parenExprI (prio <)) expr
        NSelect _ _ _ -> fmap (parenExprI (prio <=)) expr
        NHasAttr _ _ -> fmap (parenExprI (prio <=)) expr
        NList _ -> fmap (parenExprI (prio <=)) expr
        expr -> fmap exprI expr

formatI :: NExprF (NixMonad ()) -> NixMonad ()
formatI = \case
    NConstant c ->
        writeText $ atomText c
    NStr s ->
        stringI s
    NSym s ->
        writeText s
    NList vals -> do
        writeText "["
        intercalateM (writeText " ") vals
        writeText "]"
    NSet binds ->
        setI False binds
    NRecSet binds ->
        setI True binds
    NLiteralPath p ->
        writeText $ T.pack p
    NEnvPath p ->
        writeText $ T.pack $ "<" ++ p ++ ">"
    NUnary op ex ->
        writeText (unOpStr op) >> ex
    NBinary op l r ->
        l >> writeText (" " <> binOpStr op <> " ") >> r
    NSelect set attr def -> do
        set >> writeText "." >> pathI attr
        maybe noop (writeText " or " >>) def
    NHasAttr set attr -> do
        set >> writeText " ? " >> pathI attr
    NAbs (Param p) ex ->
        writeText p >> writeText ": " >> ex
    NAbs (ParamSet set name) ex -> do
        paramSetI set
        maybe noop (\n -> writeText " @ " >> writeText n) name
        writeText ": " >> ex
    NApp f x ->
        f >> writeText " " >> x
    NLet binds ex -> tryOneLine $ do
        writeText "let"
        breakableSpace >> bindersI binds
        breakableSpace >> writeText "in"
        breakableSpace >> ex
    NIf cond then_ else_ -> do
        writeText "if " >> cond
        writeText " then " >> then_
        writeText " else " >> else_
    NWith set expr -> do
        writeText "with " >> set
        writeText "; " >> expr
    NAssert test expr -> do
        writeText "assert " >> test
        writeText "; " >> expr

binderI :: Binding (NixMonad ()) -> NixMonad ()
binderI b = case b of
    NamedVar path val -> do
        pathI path >> writeText " = " >> val >> writeText ";"
    Inherit set vars -> do
        writeText "inherit "
        maybe noop (\s -> writeText "(" >> s >> writeText ") ") set
        intercalateM (writeText " ") (map keyNameI vars)
        writeText ";"

bindersI :: [Binding (NixMonad ())] -> NixMonad ()
bindersI = indent . intercalateM breakableSpace . map binderI

pathI :: NAttrPath (NixMonad ()) -> NixMonad ()
pathI p = intercalateM (writeText ".") $ map keyNameI p

keyNameI :: NKeyName (NixMonad ()) -> NixMonad ()
keyNameI kn = case kn of
    StaticKey k -> writeText k
    DynamicKey (Plain s) -> stringI s
    DynamicKey (Antiquoted e) -> do
        writeText "${"
        e
        writeText "}"

extractNString :: NString a -> [Antiquoted T.Text a]
extractNString (DoubleQuoted t) = t
extractNString (Indented t) = t

stringI :: NString (NixMonad ()) -> NixMonad ()
stringI = tryOneLine . quote . indent .
            intercalateM quotedNewLine .
            map doLine . Nix.splitLines .
            extractNString
    where
        doLine :: [Antiquoted T.Text (NixMonad ())] -> NixMonad ()
        doLine = mapM_ $ \case
            Plain t -> writeQuotedText t
            Antiquoted e -> do
                writeText "${"
                antiquote e
                writeText "}"


paramSetI :: ParamSet (NixMonad ()) -> NixMonad ()
paramSetI set = tryOneLine $ do
    writeText "{"
    breakableSpace
    indent $ do
        let (m, isVariadic) = case set of
                    { FixedParamSet m -> (m, False); VariadicParamSet m -> (m, True) }
        intercalateM
            (writeText "," >> breakableSpace)
            (map (\(k, x) -> do
                writeText k
                maybe noop (\e -> writeText " ? " >> e) x
             ) (M.toList m))
        when isVariadic $ writeText "," >> breakableSpace >> writeText "..."
    breakableSpace >> writeText "}"

setI :: Bool -> [Binding (NixMonad ())] -> NixMonad ()
setI rec binds = tryOneLine $ do
    when rec $ writeText "rec "
    writeText "{"
    when (binds /= []) $ do
        breakableSpace
        bindersI binds
        breakableSpace
    writeText "}"
