module Optimiser where

import qualified Data.Map as M
import qualified Data.Set as S
import Util
import Syntax

-- For each variable, in the 'gap' between every assignment in the program, we
-- define the ConstantPropInfo for that point and variable to be either:
data ConstantPropInfo
    -- Not ever assigned any value as far as we know
    = NotAssigned   -- (Written with the 'bottom' symbol in literature)
    -- Definitely assigned a specific known value
    | Known Int
    -- Definitely assigned to, but we can't know what value it has
    | Unknowable    -- (Written with the 'top' symbol in literature)
    deriving (Eq, Ord)

instance Show ConstantPropInfo where
    show NotAssigned = "_|_"
    show (Known x)   = show x
    show Unknowable  = "^|^"

-- Maps the names of variables in play to their corresponding ConstantPropInfo,
-- for a particular point in the program.
type InfoMap = M.Map Name ConstantPropInfo

-- A LabelledBlock bundles and InfoMap with every assignment, which stores the
-- ConstantPropInfo of the variables immediately before doing the assignment.
-- It also carries an additional InfoMap to store the ConstantPropInfo after
-- the last assignment.
newtype LabelledBlock =
    LabelledBlock ([(Assignment, InfoMap)], InfoMap, Adjacency)
    deriving (Eq, Ord)

-- Just like GraphProg is a map from block names to Blocks, a LabelledGraphProg
-- is a map from block names to LabelledBlocks.
type LabelledGraphProg = M.Map Name LabelledBlock

data OptimiseResult a
    = Result a
    | BlockNotFound Name
    | VarNotFound Name
    | IndexOutOfRange

instance Functor OptimiseResult where
    fmap f (Result x) = Result (f x)
    fmap _ (VarNotFound     blockName) = VarNotFound     blockName
    fmap _ (BlockNotFound   blockName) = BlockNotFound   blockName
    fmap _ IndexOutOfRange             = IndexOutOfRange

instance Applicative OptimiseResult where
    pure = Result
    (<*>) (Result f) (Result x) = Result (f x)
    (<*>) _ (VarNotFound     blockName) = VarNotFound     blockName
    (<*>) _ (BlockNotFound   blockName) = BlockNotFound   blockName
    (<*>) _ IndexOutOfRange             = IndexOutOfRange

instance Monad OptimiseResult where
    return = Result
    (>>=) (Result x) f = f x
    (>>=) (VarNotFound     blockName) _ = VarNotFound     blockName
    (>>=) (BlockNotFound   blockName) _ = BlockNotFound   blockName
    (>>=) IndexOutOfRange             _ = IndexOutOfRange

-- Look up a (Block, M.Map Name [ConstantPropInfo]) with given name in the given
-- LabelledGraphProg.
lgpBlockLookup ::
    Name                ->
    LabelledGraphProg   ->
    OptimiseResult LabelledBlock
lgpBlockLookup lBlockName lProg = case M.lookup lBlockName lProg of
    Nothing     -> BlockNotFound lBlockName
    Just lBlock -> Result lBlock

getAssignAt :: LabelledBlock -> Int -> OptimiseResult Assignment
getAssignAt (LabelledBlock (asmtsWithInfo, _, _)) n
    | (n >= 0) && (n < length asmtsWithInfo) = Result (fst (asmtsWithInfo !! n))
    | otherwise                              = IndexOutOfRange

getInfoAt :: LabelledBlock -> Int -> OptimiseResult InfoMap
getInfoAt (LabelledBlock (asmtsWithInfo, endInfo, _)) n
    | (n >= 0) && (n < length asmtsWithInfo) = Result (snd (asmtsWithInfo !! n))
    | n == length asmtsWithInfo              = Result endInfo
    | otherwise                              = IndexOutOfRange

-- Generate constant propagation info for a given program.
genConstantPropInfo :: GraphProg -> OptimiseResult LabelledGraphProg
genConstantPropInfo prog = updateConstantPropInfo (initialLabelling prog)

-- Label a GraphProg with 'initial' ConstantPropInfo for every assignment and
-- variable. Initially, we assume that no assignments are every reached and so
-- the labelling for every point and variable is NotAssigned.
initialLabelling :: GraphProg -> LabelledGraphProg
initialLabelling prog =
    let allNames       = varNameList prog
        allNotAssigned = M.fromList $ map (\n -> (n, NotAssigned)) allNames
    in
    M.map (initialLabellingBlock allNotAssigned) prog
    where
    initialLabellingBlock :: InfoMap -> Block -> LabelledBlock
    initialLabellingBlock allNotAssigned (Block (asmts, adj)) =
        LabelledBlock (map (\a -> (a, allNotAssigned)) asmts,
            allNotAssigned, adj)

-- Update inconsistent constant propagation information in order to obtain
-- correct information.
updateConstantPropInfo :: LabelledGraphProg -> OptimiseResult LabelledGraphProg
updateConstantPropInfo lprog = doUpdateConstantPropInfo lprog "__begin__"
    where
    -- Update inconsistent constant propagation information in order to obtain
    -- correct information, starting at the block with the given name, and
    -- proceeding recursively to its children.
    doUpdateConstantPropInfo ::
        LabelledGraphProg   ->  -- The program we're updating info on
        Name                ->  -- The name of the block we're looking at
        OptimiseResult LabelledGraphProg
    doUpdateConstantPropInfo lprog blockName = do
        block  <- lgpBlockLookup blockName lprog
        notImplemented
