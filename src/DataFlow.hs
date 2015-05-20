module DataFlow where

import qualified Data.Map      as M
import qualified Data.Sequence as Sq
import Data.Foldable (toList)
import Util
import Syntax

-- For each variable, in the 'gap' between every assignment in the program, we
-- define the DataFlowInfo for that point and variable to be either:
data DataFlowInfo
    -- Not ever assigned any value as far as we know
    = NotAssigned   -- (Written with the 'bottom' symbol in literature)
    -- Definitely assigned a specific known value
    | Known Int
    -- Definitely assigned to, but we can't know what value it has
    | Unknowable    -- (Written with the 'top' symbol in literature)
    deriving (Eq, Ord)

instance Show DataFlowInfo where
    show NotAssigned = "_|_"
    show (Known x)   = show x
    show Unknowable  = "^|^"

-- Maps the names of variables in play to their corresponding DataFlowInfo, for
-- a particular point in the program.
type InfoMap = M.Map Name DataFlowInfo

-- A LabelledBlock bundles and InfoMap with every assignment, which stores the
-- DataFlowInfo of the variables immediately before doing the assignment. It
-- also carries an additional InfoMap to store the DataFlowInfo after the last
-- assignment.
newtype LabelledBlock =
    LabelledBlock ([(Assignment, InfoMap)], InfoMap, Adjacency)
    deriving (Eq, Ord)

-- Just like GraphProg is a map from block names to Blocks, a LabelledGraphProg
-- is a map from block names to LabelledBlocks.
type LabelledGraphProg = M.Map Name LabelledBlock

data DataFlowResult a
    = Result a
    | BlockNotFound Name
    | VarNotFound Name
    | IndexOutOfRange

instance Functor DataFlowResult where
    fmap f (Result x) = Result (f x)
    fmap _ (VarNotFound     blockName) = VarNotFound     blockName
    fmap _ (BlockNotFound   blockName) = BlockNotFound   blockName
    fmap _ IndexOutOfRange             = IndexOutOfRange

instance Applicative DataFlowResult where
    pure = Result
    (<*>) (Result f) (Result x) = Result (f x)
    (<*>) _ (VarNotFound     blockName) = VarNotFound     blockName
    (<*>) _ (BlockNotFound   blockName) = BlockNotFound   blockName
    (<*>) _ IndexOutOfRange             = IndexOutOfRange

instance Monad DataFlowResult where
    return = Result
    (>>=) (Result x) f = f x
    (>>=) (VarNotFound     blockName) _ = VarNotFound     blockName
    (>>=) (BlockNotFound   blockName) _ = BlockNotFound   blockName
    (>>=) IndexOutOfRange             _ = IndexOutOfRange

-- Look up a (Block, M.Map Name [DataFlowInfo]) with given name in the given
-- LabelledGraphProg.
lgpBlockLookup ::
    Name                ->
    LabelledGraphProg   ->
    DataFlowResult LabelledBlock
lgpBlockLookup lBlockName lProg = case M.lookup lBlockName lProg of
    Nothing     -> BlockNotFound lBlockName
    Just lBlock -> Result lBlock

infoMapLookup :: Name -> InfoMap -> DataFlowResult DataFlowInfo
infoMapLookup name infoMap = case M.lookup name infoMap of
    Nothing   -> VarNotFound name
    Just info -> Result info

getAssignAt :: LabelledBlock -> Int -> DataFlowResult Assignment
getAssignAt (LabelledBlock (asmtsWithInfo, _, _)) idx
    | (idx >= 0) && (idx < length asmtsWithInfo) =
        Result (fst (asmtsWithInfo !! idx))
    | otherwise                                  = IndexOutOfRange

getInfoMapAt :: LabelledBlock -> Int -> DataFlowResult InfoMap
getInfoMapAt (LabelledBlock (asmtsWithInfo, endInfo, _)) idx
    | (idx >= 0) && (idx < length asmtsWithInfo) =
        Result (snd (asmtsWithInfo !! idx))
    | idx == length asmtsWithInfo                = Result endInfo
    | otherwise                                  = IndexOutOfRange

setInfoMapAt :: LabelledBlock -> Int -> InfoMap -> DataFlowResult LabelledBlock
setInfoMapAt (LabelledBlock (asmtsWithInfo, endInfo, adj)) idx newInfoMap
    | (idx >= 0) && (idx < length asmtsWithInfo) =
        let (asmtAtN, oldInfoMap) = asmtsWithInfo !! idx
            newAsmtsWithInfo      = toList $
                Sq.update idx (asmtAtN, newInfoMap) $ Sq.fromList asmtsWithInfo
        in
        Result (LabelledBlock (newAsmtsWithInfo, endInfo, adj))
    | idx == length asmtsWithInfo =
        Result (LabelledBlock (asmtsWithInfo, newInfoMap, adj))
    | otherwise                   = IndexOutOfRange

setInfoForNameAt :: LabelledBlock -> Int -> Name -> DataFlowInfo ->
    DataFlowResult LabelledBlock
setInfoForNameAt block idx name info =
    case block of
        (LabelledBlock (asmtsWithInfo, endInfo, adj)) -> do
            prevInfoMapAt <- getInfoMapAt block idx
            let newInfoMapAt = M.insert name info prevInfoMapAt in
                setInfoMapAt block idx newInfoMapAt

getInfoForNameAt :: LabelledBlock -> Int -> Name -> DataFlowResult DataFlowInfo
getInfoForNameAt (LabelledBlock (asmtsWithInfo, endInfo, _)) idx name
    | (idx >= 0) && (idx < length asmtsWithInfo) =
        let (asmtAtIdx, mapAtIdx) = asmtsWithInfo !! idx in do
            info <- infoMapLookup name mapAtIdx
            return info
    | idx == length asmtsWithInfo                = do
        info <- infoMapLookup name endInfo
        return info
    | otherwise                                  = IndexOutOfRange

-- Generate data flow info for a given program.
genDataFlowInfo :: GraphProg -> DataFlowResult LabelledGraphProg
genDataFlowInfo prog = initialLabelling prog >>= updateDataFlowInfo

-- Label a GraphProg with 'initial' DataFlowInfo for every assignment and
-- variable. Initially, we assume that no assignments are every reached and so
-- the labelling for every point and variable is NotAssigned.
initialLabelling :: GraphProg -> DataFlowResult LabelledGraphProg
initialLabelling prog =
    let allNames       = varNameList prog
        allNotAssigned = M.fromList $ map (\n -> (n, NotAssigned)) allNames
        allUnknowable  = M.fromList $ map (\n -> (n, Unknowable )) allNames
        allBlocksNotAssignedProg =
            M.map (initialLabellingBlock allNotAssigned) prog
    in do
        beginBlock <- lgpBlockLookup "__begin__" allBlocksNotAssignedProg
        beginBlockInitialUnknowable <- setInfoMapAt beginBlock 0 allUnknowable
        return $ M.insert "__begin__" beginBlockInitialUnknowable
                                                        allBlocksNotAssignedProg
    where
    initialLabellingBlock :: InfoMap -> Block -> LabelledBlock
    initialLabellingBlock allNotAssigned (Block (asmts, adj)) =
        LabelledBlock (map (\a -> (a, allNotAssigned)) asmts,
                                                            allNotAssigned, adj)

-- Update inconsistent data flow information in order to obtain correct
-- information.
updateDataFlowInfo :: LabelledGraphProg -> DataFlowResult LabelledGraphProg
updateDataFlowInfo lprog = doUpdateDataFlowInfo lprog "__begin__"
    where
    -- Update inconsistent data flow information in order to obtain correct
    -- information, starting at the block with the given name, and proceeding
    -- recursively to its children.
    doUpdateDataFlowInfo ::
        LabelledGraphProg   ->  -- The program we're updating info on
        Name                ->  -- The name of the block we're looking at
        DataFlowResult LabelledGraphProg
    doUpdateDataFlowInfo lprog blockName = do
        block <- lgpBlockLookup blockName lprog
        notImplemented
