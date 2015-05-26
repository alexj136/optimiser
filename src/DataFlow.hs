module DataFlow where

import qualified Data.Map as M
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

type DataFlowResult a = Either DataFlowError a

data DataFlowError
    = BlockNotFound Name
    | VarNotFound Name
    | IndexOutOfRange
    | NoRuleApplies
    deriving (Show, Eq, Ord)

-- Look up a (Block, M.Map Name [DataFlowInfo]) with given name in the given
-- LabelledGraphProg.
lgpBlockLookup ::
    Name                ->
    LabelledGraphProg   ->
    DataFlowResult LabelledBlock
lgpBlockLookup lBlockName lProg = case M.lookup lBlockName lProg of
    Nothing     -> Left (BlockNotFound lBlockName)
    Just lBlock -> return lBlock

-- Throw away data flow information to obtain an unlabelled GraphProg
discardlabels :: LabelledGraphProg -> GraphProg
discardlabels = M.map discardlabelsBlock
    where
    discardlabelsBlock :: LabelledBlock -> Block
    discardlabelsBlock (LabelledBlock (asmtsWithInfo, endInfo, adj)) =
        Block (map fst asmtsWithInfo, adj)

-- Look up an InfoMap for the DataFlowInfo corresponding to the given name
infoMapLookup :: Name -> InfoMap -> DataFlowResult DataFlowInfo
infoMapLookup name infoMap = case M.lookup name infoMap of
    Nothing   -> Left (VarNotFound name)
    Just info -> return info

-- Get the assignment statement at a particular index in a block
getAssignAt :: LabelledBlock -> Int -> DataFlowResult Assignment
getAssignAt (LabelledBlock (asmtsWithInfo, _, _)) idx
    | (idx >= 0) && (idx < length asmtsWithInfo) =
        return (fst (asmtsWithInfo !! idx))
    | otherwise                                  = Left IndexOutOfRange

-- Get the assignment statement at a particular index in a block
tryGetAssignAt :: LabelledBlock -> Int -> Maybe Assignment
tryGetAssignAt (LabelledBlock (asmtsWithInfo, _, _)) idx =
    fmap fst $ safeIndex idx asmtsWithInfo

-- Get the InfoMap at a particular index in a block
getInfoMapAt :: LabelledBlock -> Int -> DataFlowResult InfoMap
getInfoMapAt (LabelledBlock (asmtsWithInfo, endInfo, _)) idx
    | (idx >= 0) && (idx < length asmtsWithInfo) =
        return (snd (asmtsWithInfo !! idx))
    | idx == length asmtsWithInfo                = return endInfo
    | otherwise                                  = Left IndexOutOfRange

getEndInfoMap :: LabelledBlock -> InfoMap
getEndInfoMap (LabelledBlock (_, endInfo, _)) = endInfo

-- Set the InfoMap at a particular index in a block
setInfoMapAt :: LabelledBlock -> Int -> InfoMap -> DataFlowResult LabelledBlock
setInfoMapAt (LabelledBlock (asmtsWithInfo, endInfo, adj)) idx newInfoMap
    | (idx >= 0) && (idx < length asmtsWithInfo) =
        let (asmtAtN, oldInfoMap) = asmtsWithInfo !! idx
            newAsmtsWithInfo      =
                update idx (asmtAtN, newInfoMap) asmtsWithInfo
        in
        return (LabelledBlock (newAsmtsWithInfo, endInfo, adj))
    | idx == length asmtsWithInfo =
        return (LabelledBlock (asmtsWithInfo, newInfoMap, adj))
    | otherwise                   = Left IndexOutOfRange

-- Set the DataFlowInfo for a particular name at a particular index in a block
setInfoForNameAt :: LabelledBlock -> Int -> Name -> DataFlowInfo ->
    DataFlowResult LabelledBlock
setInfoForNameAt block idx name info =
    case block of
        (LabelledBlock (asmtsWithInfo, endInfo, adj)) -> do
            prevInfoMapAt <- getInfoMapAt block idx
            let newInfoMapAt = M.insert name info prevInfoMapAt in
                setInfoMapAt block idx newInfoMapAt

-- Get the DataFlowInfo for a particular name at a particular index in a block
getInfoForNameAt :: LabelledBlock -> Int -> Name -> DataFlowResult DataFlowInfo
getInfoForNameAt (LabelledBlock (asmtsWithInfo, endInfo, _)) idx name
    | (idx >= 0) && (idx < length asmtsWithInfo) =
        let (asmtAtIdx, mapAtIdx) = asmtsWithInfo !! idx in do
            info <- infoMapLookup name mapAtIdx
            return info
    | idx == length asmtsWithInfo                = do
        info <- infoMapLookup name endInfo
        return info
    | otherwise                                  = Left IndexOutOfRange

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
updateDataFlowInfo lProg = notImplemented

-- Update inconsistent data flow information within a single block
updateDataFlowInfoBlock ::
    LabelledGraphProg ->             -- The initial program
    Name              ->             -- The name of the block to update info in
    Name              ->             -- The variable name we're intersted in
    DataFlowResult LabelledGraphProg -- The resulting program
updateDataFlowInfoBlock lProg blockName varName = notImplemented

-- Given an InfoMap reference (via its program, block name and index in its
-- block), get the InfoMap object for the program point immediately before that
-- statement. For InfoMaps that are not the first in the block, we just return
-- the InfoMap at the previous index in a singleton list. For those at the
-- beginning of the block, we return a list of InfoMaps, one for each InfoMap at
-- the end of every block that goes to the given block.
getPrevInfoMaps :: LabelledGraphProg -> Name -> Int -> DataFlowResult [InfoMap]
getPrevInfoMaps lProg blockName 0   =
    let prevBlockNames = predecessors (discardlabels lProg) blockName
    in do
    blocks  <- mapM (\bN -> lgpBlockLookup bN lProg) prevBlockNames
    return (map getEndInfoMap blocks)
getPrevInfoMaps lProg blockName idx = do
    block   <- lgpBlockLookup blockName lProg
    infoMap <- getInfoMapAt block (idx - 1)
    return [infoMap]

updateDataFlowInfoAtPoint ::
    LabelledGraphProg ->             -- The initial program
    Name              ->             -- The name of the block to update info in
    Int               ->             -- The index of the InfoMap in the block
    Name              ->             -- The variable name we're intersted in
    DataFlowResult LabelledGraphProg -- The resulting program
updateDataFlowInfoAtPoint lProg blockName idx varName = do
    block          <- lgpBlockLookup blockName lProg
    prevInfoMaps   <- getPrevInfoMaps lProg blockName idx
    prevInfo       <- mapM (infoMapLookup varName) prevInfoMaps
    assignment     <- return $ tryGetAssignAt block (idx - 1)
    resultantInfo  <- applyUpdateRules prevInfo assignment
    resultantBlock <- setInfoForNameAt block idx varName resultantInfo
    return $ M.insert blockName resultantBlock lProg

-- The following rules take a list of DataFlowInfo, containing the DataFlowInfo
-- after every statement that can immediately preceed the given statement, and
-- return the DataFlowInfo after the statement.
applyUpdateRules ::
    [DataFlowInfo]      ->  -- the DataFlowInfo after all preceeding statements
    Maybe Assignment    ->  -- an Assignment that may preceed the output
                            -- DataFlowInfo, that proceeds immediately after the
                            -- given DataFlowInfo list holds.
    DataFlowResult DataFlowInfo
applyUpdateRules prevInfo maybeAssignment
    -- If the value is unknowable on any of the paths into the statement,
    -- then the value is unknowable after the statement
    | any (== Unknowable) prevInfo = return Unknowable

    -- If a value has not been assigned on every path into the statement,
    -- then it has not been assigned after the statement
    | all (== NotAssigned) prevInfo = return NotAssigned

    -- If all paths into the statement are not unknowable, and ignoring
    -- paths where the value is not assigned (the previous rule guarantees
    -- that there is at least one path where it IS assigned), and those
    -- paths have a known values, but disagree on what the value is, then
    -- the value is unknowable after the statement
    | not (allSame (filter (/= NotAssigned) prevInfo)) = return Unknowable

    -- The converse of the previous case - no unknowables, and all known
    -- values agree in value, then after the statement, the value is known
    -- to have the agreed upon value
    | allSame (filter (/= NotAssigned) prevInfo) =
        return $ head (filter (/= NotAssigned) prevInfo)

    | otherwise = Left NoRuleApplies

{- DATA FLOW INFORMATION PROPAGATION RULES
info :: VarName -> Statement -> (In | Out) -> (_|_ | Const Int | ^|^)
info x stmt In =
    let predsInfo = map (\s -> info x s Out) (preds stmt) in
        | any (== ^|^) predsInfo = ^|^
        | notAllSame   predsInfo = ^|^
        | all (== (Const C | _|_)) predsInfo = Const C        -- All Cs the same
        | all (== _|_) predsInfo = _|_
info x stmt Out =
    | info x stmt In == _|_ = _|_
info x (x := C) Out = Const C
info x (x := y + z) Out = ^|^
info x (y := C) Out =
    | x =/= y = info x (y := C) In
-}
