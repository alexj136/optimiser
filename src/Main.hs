module Main where

import qualified Data.Map as M
import System.Environment (getArgs)
import Lexer
import Parser
import Syntax
import Interpreter

main :: IO ()
main = do
    args <- getArgs
    if length args == 1 then do
        fileText <- readFile (args !! 0)
        let tokens = scan fileText
            linearAST = parse tokens
            graphAST  = linearToGraph linearAST
            resultStr = case interpret graphAST M.empty of
                Result map                     -> show map
                EvaluationOfUndefinedName name ->
                    "Variable name '" ++ name ++ "' undefined"
                JumpToUndefinedLabel      name ->
                    "Label '" ++ name ++ "' undefined"
            in
            putStrLn resultStr
    else
        putStrLn "Illegal argument(s)"
