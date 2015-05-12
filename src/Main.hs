module Main where

import Lexer
import Parser
import Syntax

main :: IO ()
main = do
    args <- getArgs
    putStrLn . ppLinearProg . parse . scan . readFile
