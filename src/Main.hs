module Main where

import Data.Either
import Data.List (intersperse)
import qualified Data.Map    as M
import System.Environment (getArgs)
import System.Exit
import Util
import Lexer
import Parser
import Syntax
import qualified Interpreter as I
import qualified DataFlow    as Df
import qualified ConstProp   as Cp

main :: IO ExitCode
main = do
    args        <- getArgs
    parseResult <- return $ argParse args
    resultStr   <- case parseResult of
        Right actionAndFileName         -> doAction actionAndFileName
        Left InvalidCmdSyntax           -> return helpString
        Left (FileNotOpenable fileName) ->
            return $ "File '" ++ fileName ++ "' could not be opened."
    return $ case parseResult of
        Left  _ -> ExitFailure 1
        Right _ -> ExitSuccess

-- The things that Main can do
data Action
    = RunOptimised
    | RunUnoptimised
    | ShowOptimise
    deriving Eq

data CmdError
    = FileNotOpenable String
    | InvalidCmdSyntax
    deriving Eq

type CmdLineResult = Either CmdError (Action, String)

getProg :: IO String -> IO GraphProg
getProg iOFileName = do
    fileName <- iOFileName
    fileText <- readFile fileName
    return $ linearToGraph $ parse $ scan fileText

doAction :: (Action, String) -> IO String
doAction (action, fileText) = case action of
    RunOptimised   -> notImplemented
    RunUnoptimised -> notImplemented
    ShowOptimise   -> notImplemented

argParse :: [String] -> CmdLineResult
argParse args = case args of

    [fileName          ] -> Right (RunOptimised, fileName)
    [fileName, "--run" ] -> Right (RunOptimised, fileName)
    [fileName, "-r"    ] -> Right (RunOptimised, fileName)
    ["--run" , fileName] -> Right (RunOptimised, fileName)
    ["-r"    , fileName] -> Right (RunOptimised, fileName)

    [fileName           , "--run-unoptimised"] ->
        Right (RunUnoptimised, fileName)
    [fileName           , "-u"               ] ->
        Right (RunUnoptimised, fileName)
    ["--run-unoptimised", fileName           ] ->
        Right (RunUnoptimised, fileName)
    ["-u"               , fileName           ] ->
        Right (RunUnoptimised, fileName)

    [fileName         , "--show-optimise"] -> Right (ShowOptimise, fileName)
    [fileName         , "-s"             ] -> Right (ShowOptimise, fileName)
    ["--show-optimise", fileName         ] -> Right (ShowOptimise, fileName)
    ["-s"             , fileName         ] -> Right (ShowOptimise, fileName)

    _ -> Left InvalidCmdSyntax

helpString :: String
helpString = concat $ intersperse "\n" $
    [ "----------------------------------------------------------"
    , "| COMMAND ARGUMENTS          | ACTION                    |"
    , "----------------------------------------------------------"
    , "| filename                   |                           |"
    , "| filename --run             | Run 'filename'            |"
    , "| filename -r                | with optimsiation enabled |"
    , "| --run filename             |                           |"
    , "| -r filename                |                           |"
    , "----------------------------------------------------------"
    , "| filename --run-unoptimised |                           |"
    , "| filename -u                | Run 'filename'            |"
    , "| --run-unoptimised filename | with no optimisation      |"
    , "| -u filename                |                           |"
    , "----------------------------------------------------------"
    , "| filename --show-optimise   |                           |"
    , "| filename -s                | Display an optimised      |"
    , "| --show-optimise filename   | version of 'filename'     |"
    , "| -s filename                |                           |"
    , "----------------------------------------------------------"
    , "| -h                         |                           |"
    , "| --help                     | Print this message        |"
    , "| (anything else)            |                           |"
    , "----------------------------------------------------------"
    ]
