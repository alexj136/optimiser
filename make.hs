#!/usr/bin/runhaskell

import Data.List (intersperse)
import System.Environment (getArgs)
import System.Process (system)

main = do
    args <- getArgs
    system $ concat $ intersperse " " $ "echo" : args
