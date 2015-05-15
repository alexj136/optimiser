module Optimiser where

import qualified Data.Map as M
import Util
import Syntax

type LabelledGraphProg = M.Map Name (Block, M.Map Name [ConstantPropInfo])

data ConstantPropInfo
    = NotAssigned   -- Bottom
    | Known Int
    | Unknowable    -- Top
    deriving (Eq, Ord)

data OptimiserResult a
    = Result a
    | BlockNotFound Name
    | VarNotFound Name

-- Look up the labellings for a given variable, for the stages in a given block
labelLookup ::
    LabelledGraphProg   ->              -- The program to look up
    Name                ->              -- The block we're interested in
    Name                ->              -- The variable we're interested in
    OptimiserResult [ConstantPropInfo]  -- The corresponding info
labelLookup lprog blockName varName = case M.lookup blockName lprog of
    Nothing              -> BlockNotFound blockName
    Just (_, conPropMap) -> case M.lookup varName conPropMap of
        Nothing       -> VarNotFound varName
        Just infoList -> Result infoList

-- Generate constant propagation info for a given program.
genConstantPropInfo :: GraphProg -> OptimiserResult LabelledGraphProg
genConstantPropInfo = updateConstantPropInfo . initialLabelling

-- Label a GraphProg with 'initial' ConstantPropInfo for every assignment and
-- variable. Initially, we assume that no assignments are every reached and so
-- the labelling for every point and variable is NotAssigned.
initialLabelling :: GraphProg -> LabelledGraphProg
initialLabelling = fmap (\blk -> (blk, M.empty))

-- Update inconsistent constant propagation information in order to obtain
-- correct information.
updateConstantPropInfo :: LabelledGraphProg -> OptimiserResult LabelledGraphProg
updateConstantPropInfo lprog = doUpdateConstantPropInfo lprog "__begin__"
    where
    doUpdateConstantPropInfo ::
        LabelledGraphProg   ->
        Name                ->
        OptimiserResult LabelledGraphProg
    doUpdateConstantPropInfo lprog blockName = notImplemented
