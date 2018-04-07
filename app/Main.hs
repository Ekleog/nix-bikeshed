module Main where

import Lib

import Options.Applicative
import Data.Monoid((<>))

data Options = Options {
    file :: String,
    check :: Bool
}

parseOptions :: Parser Options
parseOptions = Options
    <$> argument str
         ( metavar "STRING"
        <> help "File to pretty-print" )
    <*> switch
         ( long "check"
        <> help "Whether to check the transformation was AST-wise perfect" )

main :: IO ()
main = do
        Options file check <- execParser options
        doTheThing file check
    where
        options = info (helper <*> parseOptions)
             ( fullDesc
            <> progDesc "Pretty-prints a Nix file"
            <> header "nix-bikeshed - the one stop for all your painting" )
