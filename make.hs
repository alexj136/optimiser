#!/usr/bin/runhaskell

import System.Environment (getArgs)
import System.Process (system)

main :: IO ()
main = do
    args <- getArgs
    system $ if args == ["clean"] then
        "rm -rf ./bin ./src/Lexer.hs ./src/Parser.hs run"
    else if args == [] then
        " mkdir -p ./bin        &&\
        \ alex src/Lexer.x      &&\
        \ happy src/Parser.y    &&\
        \ ghc src/Main -o run   "
    else
        "echo Illegal argument"
    return ()
