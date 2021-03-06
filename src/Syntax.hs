module Syntax where

import Util (notImplemented)
import Data.List (intersperse)
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
data Assignment
    = FromOne Name Val
    | FromTwo Name Val Op Val
    deriving (Eq, Ord)

assignee :: Assignment -> Name
assignee (FromOne n _)     = n
assignee (FromTwo n _ _ _) = n

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
successors :: Block -> [Name]
successors (Block (_, AdjGoto n))     = [n]
successors (Block (_, AdjIf _ n1 n2)) = [n1, n2]
numAssigns :: Block -> Int
numAssigns = length . assignments

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

-- Obtain the names of all blocks that lead immediatley to the given block, by
-- way of a direct or a conditional jump.
predecessors ::
    GraphProg   -> -- The containing program
    Name        -> -- The name of the block for which we're interested in
    [Name]
predecessors prog blockName =
    M.keys $ M.filter (\blk -> any (blockName ==) (successors blk)) prog

-- Enumerate the variable names (not label names) in a GraphProg
varNameList :: GraphProg -> [Name]
varNameList = (map assignee) . concat . (map assignments) . M.elems

{-------------------------------------------------------------------------------
                            LINEAR TO GRAPH CONVERSION
-------------------------------------------------------------------------------}

-- Convert a linear program to a graph program. Include __begin__ and __end__
-- labels to aid the interpreter in execution.
linearToGraph :: LinearProg -> GraphProg
linearToGraph linProg = linearToGraphCont 0 M.empty
    ([Label "__begin__"] ++ linProg ++ [Label "__end__"])
    where
    linearToGraphCont :: Int -> GraphProg -> LinearProg -> GraphProg
    linearToGraphCont newName curGraphProg curLinearProg =
        case curLinearProg of
            [] -> curGraphProg
            (Label name : rest) ->
                let (block, afterBlock) = parseBlock rest []
                in
                linearToGraphCont newName (M.insert name block curGraphProg)
                                                                afterBlock
            (stmt : rest) ->
                let nextNewName = newName + 1
                    newNameStr = "__" ++ (show newName) ++ "__"
                in
                linearToGraphCont nextNewName curGraphProg
                                            (Label newNameStr : stmt : rest)

parseBlock :: LinearProg -> [Assignment] -> (Block, LinearProg)
parseBlock []            curBlock = (Block (curBlock, AdjGoto "__end__"), [])
parseBlock (stmt : rest) curBlock = case stmt of
    Label name          -> (Block (curBlock, AdjGoto name), stmt : rest)
    If val tName fName  -> (Block (curBlock, AdjIf val tName fName), rest)
    Goto name           -> (Block (curBlock, AdjGoto name), rest)
    Assign asmt         -> parseBlock rest (curBlock ++ [asmt])

graphToLinear :: GraphProg -> LinearProg
graphToLinear graphProg = let
    beginBlock     = graphProg M.! "__begin__"
    endBlock       = graphProg M.! "__end__"
    progNoBeginEnd = M.delete "__begin__" $ M.delete "__end__" graphProg
    in
    concat $ map blockAndNameToLinear $ concat $
        [ [ ("__begin__", beginBlock) ]
        , M.toList progNoBeginEnd
        , [ ("__end__"  , endBlock  ) ]
        ]

blockAndNameToLinear :: (Name, Block) -> [LinearStatement]
blockAndNameToLinear (label, Block (asmts, adj)) = concat $
    [ [ Label label ]
    , map Assign asmts
    , [ case adj of { AdjGoto n -> Goto n ; AdjIf v n1 n2 -> If v n1 n2 } ]
    ]

{-------------------------------------------------------------------------------
                                PRETTY PRINTING
-------------------------------------------------------------------------------}

instance Show Op where
    show op = case op of { Add -> "+" ; Sub -> "-" ; Mul -> "*" }

instance Show Val where
    show val = case val of { Var name -> name ; Lit int -> show int }

instance Show Assignment where
    show assignment = concat $ intersperse " " $ case assignment of
        FromOne name val      -> [name, ":=", show val]
        FromTwo name v1 op v2 -> [name, ":=", show v1, show op, show v2]

instance Show LinearStatement where
    show (Label name)   = name ++ ":"
    show (If val n1 n2) =
        concat $ intersperse " " $ ["if", show val, "goto", n1, "else", n2]
    show (Goto name)    = "goto " ++ name
    show (Assign asmt)  = show asmt

instance Show Adjacency where
    show (AdjGoto name)          = show (Goto name)
    show (AdjIf val name1 name2) = show (If val name1 name2)

ppLinearProg :: LinearProg -> String
ppLinearProg = concat . intersperse "\n" . map show
