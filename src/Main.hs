module Main where

import qualified Data.Map    as M
import System.Environment (getArgs)
import Util
import Lexer
import Parser
import Syntax
import qualified Interpreter as I
import qualified DataFlow    as Df

main :: IO ()
main = do
    args <- getArgs
    if length args == 1 then
        do {
            fileText <- readFile (args !! 0);
            let tokens = scan fileText
                linearAST = parse tokens
                graphAST  = linearToGraph linearAST
            in
            putStrLn $ concat [
                I.ppInterpretResult (I.interpret graphAST M.empty),
                "\n\n",
                Df.ppDataFlowResult (Df.genDataFlowInfo graphAST)]
        }
    else
        putStrLn "Illegal argument(s)"
