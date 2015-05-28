-- This module implements interpretation of programs. We interpret only
-- GraphProgs, and do not interpret LinearProgs. The interpretation function,
-- doBlock, has type GraphProg -> M.Map Name Int -> M.Map Name Int. Thus we
-- consider the program argument to be an initial variable store, and the result
-- a final variable store.
module Interpreter where

import Syntax
import Control.Monad (foldM)
import qualified Data.Map as M
import Data.List (intersperse)

{-------------------------------------------------------------------------------
                            INTERPRETER ERROR MONAD
-------------------------------------------------------------------------------}

type InterpretResult a = Either InterpretError a

data InterpretError
    -- An error due to an undefined variable
    = EvaluationOfUndefinedName Name
    -- An error due to an undefined label
    | JumpToUndefinedLabel Name

{-------------------------------------------------------------------------------
                                PRETTY PRINTING
-------------------------------------------------------------------------------}

instance Show InterpretError where
    show (JumpToUndefinedLabel      name) = "Label '" ++ name ++ "' undefined"
    show (EvaluationOfUndefinedName name) =
        "Variable name '" ++ name ++ "' undefined"

ppInterpretResult :: InterpretResult (M.Map Name Int) -> String
ppInterpretResult res = case res of
    Left  err -> show err
    Right map -> ppVarMap map

ppVarMap :: M.Map Name Int -> String
ppVarMap = concat . intersperse ", " . map ppVarValuePair . M.toList

ppVarValuePair :: (Name, Int) -> String
ppVarValuePair (n, i) = n ++ " = " ++ show i

{-------------------------------------------------------------------------------
                            INTERPRETER FUNCTIONS
-------------------------------------------------------------------------------}

-- Maps with Name keys play an important role in the interpreter. We often have
-- to lookup a named thing in map and then use the thing. However, if the map
-- doesn't contain what we're looking for, we fail. And we may fail in different
-- ways depending on the type of thing we're looking up. iLookup takes the error
-- function to use if the thing is not present, looks up the name in the map,
-- using the given error function if the thing isn't present.
iLookup ::
    (Name -> InterpretError)    ->  -- The error function
    Name                        ->  -- The name to query with
    M.Map Name a                ->  -- The map to query
    InterpretResult a               -- The value we look up
iLookup errFn name map = case M.lookup name map of
    Nothing -> Left (errFn name)
    Just a  -> return a

interpret ::
    GraphProg       ->                  -- The program we're executing
    M.Map Name Int  ->                  -- The initial store
    InterpretResult (M.Map Name Int)    -- The final store
interpret prog initialStore = interpret' prog "__begin__" initialStore
    where
    interpret' prog "__end__" store = return store
    interpret' prog curBlockName store = do
        (nextLabel, newStore) <- doBlock prog curBlockName store
        interpret' prog nextLabel newStore
    
-- Step through a part of the program by executing all assignments in one basic
-- block, and jumping to the next basic block.
doBlock ::
    GraphProg       ->                      -- The program we're executing
    Name            ->                      -- The name of the block we're
                                            -- currently executing
    M.Map Name Int  ->                      -- The current variable store
    InterpretResult (Name, M.Map Name Int)  -- The resulting store and location
doBlock prog curBlockName store = do
    block           <- (iLookup JumpToUndefinedLabel) curBlockName prog
    storeAfterBlock <- doAssignments store (assignments block)
    nextLabel       <- followAdjacency prog (adjacency block) storeAfterBlock
    return (nextLabel, storeAfterBlock)

-- Given an program, an adjacency in that program, and the current store,
-- determine the label name that we should follow the adjacency to.
followAdjacency ::
    GraphProg       ->      -- The program we're executing
    Adjacency       ->      -- The adjacency we're to follow
    M.Map Name Int  ->      -- The current variable store
    InterpretResult Name    -- The label of the next block to execute
followAdjacency prog adj store = case adj of
    AdjGoto lbl         -> return lbl
    AdjIf val lbl1 lbl2 -> do
        ev <- evalVal val store
        return (if ev > 0 then lbl1 else lbl2)

-- Apply a list of assignments in sequence in a given store. Takes the list of
-- assignments and the initial store as arguments and returns the resulting
-- store.
doAssignments :: M.Map Name Int -> [Assignment] ->
    InterpretResult (M.Map Name Int)
doAssignments = foldM doAssignment

-- Apply a single assignment in a given store. Takes the assignment operation
-- and the initial store as arguments and returns the resulting store.
doAssignment :: M.Map Name Int -> Assignment -> InterpretResult (M.Map Name Int)
doAssignment store (FromOne name val) = do
    ev <- evalVal val store
    return (M.insert name ev store)
doAssignment store (FromTwo name v1 op v2) = do
    ev1 <- evalVal v1 store
    ev2 <- evalVal v2 store
    return (M.insert name ((opToFunc op) ev1 ev2) store)

-- Evaluate a 'value'. Remember that values are restricted expressions that can
-- be integer literals or variables. To evaluate one, just return the literal if
-- it's a literal, or look up the variable value in the store if it's a
-- variable.
evalVal ::
    Val ->              -- The value to evaluate
    M.Map Name Int ->   -- The current store
    InterpretResult Int -- The 'evaluated' value
evalVal (Lit int)  store = return int
evalVal (Var name) store = (iLookup EvaluationOfUndefinedName) name store
