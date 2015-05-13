module Main where

import Lexer
import Parser
import Syntax
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if length args == 1 then do
        fileText <- readFile (args !! 0)
        let tokens = scan fileText
            ast    = parse tokens
            ppStr  = ppLinearProg ast in
            putStrLn $ concat $ [show tokens, "\n", ppStr]
    else
        putStrLn "Illegal argument(s)"
