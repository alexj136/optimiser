module Syntax where

import qualified Data.Map as M

-- We represent names as strings.
type Name = String

-- The operations supported by the language.
data Op
    = Add
    | Sub
    | Mul
    deriving (Eq, Ord)

opToFunc :: Op -> (Int -> Int -> Int)
opToFunc Add = (+)
opToFunc Sub = (-)
opToFunc Mul = (*)

-- Values can be integer literals or variable names. These can be thought of as
-- very basic expressions.
data Val
    = Var Name
    | Lit Int
    deriving (Eq, Ord)

-- Assignments are the basic units of computation. They are three-address
-- statements (not SSA though). They assign a literal to a name, which is
-- computed from two other values via an operation.
newtype Assignment
    = Assignment (Name, Val, Op, Val)
    deriving (Eq, Ord)

-- We can think of a program as a list of assignments, labels, jumps and
-- conditional jumps. We move down the list, performing the assignments, and
-- taking the jumps, until we reach the end.
data LinearStatement
    = Label Name
    | If Val Name Name
    | Goto Name
    | Assign Assignment
    deriving (Eq, Ord)

type LinearProg = [LinearStatement]

newtype Block
    = Block ([Assignment], Adjacency)
    deriving (Eq, Ord)

assignments :: Block -> [Assignment]
assignments (Block (assignmentList, _)) = assignmentList
adjacency :: Block -> Adjacency
adjacency (Block (_, adj)) = adj

-- An ajacency describes the mode of exit from a block and transition to another
-- block, which can happen in two ways:
data Adjacency
    -- Exit the block with an unconditional jump to a block with a given name
    = AdjGoto Name
    -- Exit the block with a conditional jump to a given name if val > 0, or a
    -- different given name otherwise
    | AdjIf Val Name Name
    deriving (Eq, Ord)

type GraphProg  = M.Map Name Block

linearToGraphCont :: LinearProg -> Int -> GraphProg -> GraphProg
linearToGraphCont []            newName curGraphProg = curGraphProg
linearToGraphCont (stmt : rest) newName curGraphProg = case stmt of
    Label name ->
        let (block, afterBlock) = parseBlock rest [] in
        linearToGraphCont afterBlock newName (M.insert name block curGraphProg)
    _ ->
        let nextNewName = newName + 1
            newNameStr = "__" ++ (show newName) ++ "__"
        in
        linearToGraphCont (Label newNameStr : stmt : rest) nextNewName curGraphProg

parseBlock :: LinearProg -> [Assignment] -> (Block, LinearProg)
parseBlock []            curBlock = (Block (curBlock, AdjGoto "__end__"), [])
parseBlock (stmt : rest) curBlock = case stmt of
    Label name          -> (Block (curBlock, AdjGoto name), stmt : rest)
    If val tName fName  -> (Block (curBlock, AdjIf val tName fName), rest)
    Goto name           -> (Block (curBlock, AdjGoto name), rest)
    Assign asmt         -> parseBlock rest (curBlock ++ [asmt])
