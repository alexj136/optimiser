module Optimiser where

import qualified Data.Map as M
import qualified Data.Set as S
import Util
import Syntax

type LabelledGraphProg = M.Map Name (Block, M.Map Name [ConstantPropInfo])

data ConstantPropInfo
    = NotAssigned   -- Bottom
    | Known Int
    | Unknowable    -- Top
    deriving (Eq, Ord)

data OptimiseResult a
    = Result a
    | BlockNotFound Name
    | VarNotFound Name

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
genConstantPropInfo = updateConstantPropInfo . initialLabelling

-- Label a GraphProg with 'initial' ConstantPropInfo for every assignment and
-- variable. Initially, we assume that no assignments are every reached and so
-- the labelling for every point and variable is NotAssigned.
initialLabelling :: GraphProg -> LabelledGraphProg
initialLabelling = fmap (\blk -> (blk, M.empty))

-- Update inconsistent constant propagation information in order to obtain
-- correct information.
updateConstantPropInfo :: LabelledGraphProg -> OptimiseResult LabelledGraphProg
updateConstantPropInfo lprog = doUpdateConstantPropInfo lprog "__begin__"
    where
    doUpdateConstantPropInfo ::
        LabelledGraphProg   ->
        Name                ->
        OptimiseResult LabelledGraphProg
    doUpdateConstantPropInfo lprog blockName = do
        block  <- lgpBlockLookup blockName lprog
        labels <- labelLookup blockName notImplemented {--varName -} lprog
        notImplemented
