module Lib (doTheThing) where

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
indent a = show a
