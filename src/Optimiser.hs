module Optimiser where

import qualified Data.Map as M
import qualified Data.Set as S
import Util
import Syntax

type LabelledGraphProg = M.Map Name (Block, M.Map Name [ConstantPropInfo])

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

data OptimiseResult a
    = Result a
    | BlockNotFound Name
    | VarNotFound Name

data BeforeOrAfter = Before | After deriving (Show, Eq, Ord)

instance Functor OptimiseResult where
    fmap f (Result x) = Result (f x)
    fmap f (VarNotFound   name) = VarNotFound   name
    fmap f (BlockNotFound name) = BlockNotFound name

instance Applicative OptimiseResult where
    pure = Result
    (<*>) (Result f) (Result x) = Result (f x)
    (<*>) _ (VarNotFound   name) = VarNotFound   name
    (<*>) _ (BlockNotFound name) = BlockNotFound name

instance Monad OptimiseResult where
    return = Result
    (>>=) (Result x) f = f x
    (>>=) (VarNotFound   name) _ = VarNotFound   name
    (>>=) (BlockNotFound name) _ = BlockNotFound name

-- Look up the labellings for a given variable, for the stages in a given block
labelLookup ::
    Name                ->              -- The variable we're interested in
    Name                ->              -- The block we're interested in
    LabelledGraphProg   ->              -- The program to look up
    OptimiseResult [ConstantPropInfo]   -- The corresponding info
labelLookup varName blockName lprog = do
    (_, conPropMap) <- lgpBlockLookup blockName lprog
    case M.lookup varName conPropMap of
        Nothing       -> VarNotFound varName
        Just infoList -> Result infoList

-- Look up a (Block, M.Map Name [ConstantPropInfo]) with given name in the given
-- LabelledGraphProg.
lgpBlockLookup ::
    Name                ->
    LabelledGraphProg   ->
    OptimiseResult (Block, M.Map Name [ConstantPropInfo])
lgpBlockLookup blockName lprog = case M.lookup blockName lprog of
    Nothing        -> BlockNotFound blockName
    Just blkAndCPI -> Result blkAndCPI

-- Generate constant propagation info for a given program.
genConstantPropInfo :: GraphProg -> OptimiseResult LabelledGraphProg
genConstantPropInfo prog = do
    initiallyLabelledProg <- initialLabelling prog
    updateConstantPropInfo initiallyLabelledProg

-- Label a GraphProg with 'initial' ConstantPropInfo for every assignment and
-- variable. Initially, we assume that no assignments are every reached and so
-- the labelling for every point and variable is NotAssigned.
initialLabelling :: GraphProg -> OptimiseResult LabelledGraphProg
initialLabelling prog =
    let allNames       = varNameSet prog
        allNotAssigned = fmap (\blk -> (blk, M.empty)) prog in do
        beginBlock    <- lgpBlockLookup "__begin__" allNotAssigned
        notImplemented

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
        labels <- labelLookup blockName notImplemented {- varName -} lprog
        notImplemented
