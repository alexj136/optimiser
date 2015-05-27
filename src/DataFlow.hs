module DataFlow where

import Control.Monad (foldM)
import qualified Data.Map as M
import Util
import Syntax

{-------------------------------------------------------------------------------
                    DATA TYPES FOR DATA FLOW LABELS
-------------------------------------------------------------------------------}

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

-- Maps the names of variables in play to their corresponding DataFlowInfo, for
-- a particular point in the program.
type InfoMap = M.Map Name DataFlowInfo

-- A LabelledBlock bundles and InfoMap with every assignment, which stores the
-- DataFlowInfo of the variables immediately before doing the assignment. It
-- also carries an additional InfoMap to store the DataFlowInfo after the last
-- assignment.
newtype LabelledBlock =
    LabelledBlock ([(InfoMap, Assignment)], InfoMap, Adjacency)
    deriving (Eq, Ord)

-- Just like GraphProg is a map from block names to Blocks, a LabelledGraphProg
-- is a map from block names to LabelledBlocks.
type LabelledGraphProg = M.Map Name LabelledBlock

{-------------------------------------------------------------------------------
                     LABELLING OPERATION ERROR MONAD
-------------------------------------------------------------------------------}

type DataFlowResult a = Either DataFlowError a

data DataFlowError
    = BlockNotFound Name
    | VarNotFound Name
    | IndexOutOfRange
    | NoRuleApplies
    deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
                                PRETTY PRINTING
-------------------------------------------------------------------------------}

instance Show DataFlowInfo where
    show NotAssigned = "_|_"
    show (Known x)   = show x
    show Unknowable  = "^|^"

instance Show LabelledBlock where

{-------------------------------------------------------------------------------
                    GETTERS/SETTERS FOR LABELLED PROGRAMS
-------------------------------------------------------------------------------}

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
        Block (map snd asmtsWithInfo, adj)

-- Look up an InfoMap for the DataFlowInfo corresponding to the given name
infoMapLookup :: Name -> InfoMap -> DataFlowResult DataFlowInfo
infoMapLookup name infoMap = case M.lookup name infoMap of
    Nothing   -> Left (VarNotFound name)
    Just info -> return info

-- Get the assignment statement at a particular index in a block
getAssignAt :: LabelledBlock -> Int -> DataFlowResult Assignment
getAssignAt (LabelledBlock (asmtsWithInfo, _, _)) idx
    | (idx >= 0) && (idx < length asmtsWithInfo) =
        return (snd (asmtsWithInfo !! idx))
    | otherwise                                  = Left IndexOutOfRange

-- Get the InfoMap at a particular index in a block
getInfoMapAt :: LabelledBlock -> Int -> DataFlowResult InfoMap
getInfoMapAt (LabelledBlock (asmtsWithInfo, endInfo, _)) idx
    | (idx >= 0) && (idx < length asmtsWithInfo) =
        return (fst (asmtsWithInfo !! idx))
    | idx == length asmtsWithInfo                = return endInfo
    | otherwise                                  = Left IndexOutOfRange

getEndInfoMap :: LabelledBlock -> InfoMap
getEndInfoMap (LabelledBlock (_, endInfo, _)) = endInfo

-- Set the InfoMap at a particular index in a block
setInfoMapAt :: LabelledBlock -> Int -> InfoMap -> DataFlowResult LabelledBlock
setInfoMapAt (LabelledBlock (asmtsWithInfo, endInfo, adj)) idx newInfoMap
    | (idx >= 0) && (idx < length asmtsWithInfo) =
        let (oldInfoMap, asmtAtN) = asmtsWithInfo !! idx
            newAsmtsWithInfo      =
                update idx (newInfoMap, asmtAtN) asmtsWithInfo
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
        let (mapAtIdx, asmtAtIdx) = asmtsWithInfo !! idx in do
            info <- infoMapLookup name mapAtIdx
            return info
    | idx == length asmtsWithInfo                = do
        info <- infoMapLookup name endInfo
        return info
    | otherwise                                  = Left IndexOutOfRange

-- Given a block name (and it's program), retrieve the end info maps for all
-- blocks that can lead into that block.
getPrevInfoMaps :: LabelledGraphProg -> Name -> DataFlowResult [InfoMap]
getPrevInfoMaps lProg blockName =
    let prevBlockNames = predecessors (discardlabels lProg) blockName
    in do
    blocks  <- mapM (\bN -> lgpBlockLookup bN lProg) prevBlockNames
    return (map getEndInfoMap blocks)

{-------------------------------------------------------------------------------
                    DATA FLOW INFORMATION GENERATION
-------------------------------------------------------------------------------}

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
        LabelledBlock (map (\a -> (allNotAssigned, a)) asmts,
                                                            allNotAssigned, adj)

-- Update inconsistent data flow information in order to obtain correct
-- information.
updateDataFlowInfo :: LabelledGraphProg -> DataFlowResult LabelledGraphProg
updateDataFlowInfo lProg = notImplemented

-- Update inconsistent data flow information within a single block, for all
-- names.
updateDataFlowInfoBlock ::
    LabelledGraphProg ->             -- The initial program
    Name              ->             -- The name of the block to update info in
    DataFlowResult LabelledGraphProg -- The resulting program
updateDataFlowInfoBlock lProg blockName = do
    block <- lgpBlockLookup blockName lProg
    foldM (\lP -> updateDataFlowInfoAtPoint lP blockName) lProg
                                                            (infoIndices block)
    where
    infoIndices :: LabelledBlock -> [Int]
    infoIndices (LabelledBlock (asmtsWithInfo, _, _)) =
        [0..(length asmtsWithInfo)]

-- Update inconsistent data flow information for all names, at a single program
-- point.
updateDataFlowInfoAtPoint ::
    LabelledGraphProg ->             -- The initial program
    Name              ->             -- The name of the block to update info in
    Int               ->             -- The index at which to update
    DataFlowResult LabelledGraphProg -- The resulting program
updateDataFlowInfoAtPoint lProg blockName idx =
    foldM (\lP -> updateDataFlowInfoAtPointForName lP blockName idx) lProg
                                            (varNameList (discardlabels lProg))

updateDataFlowInfoAtPointForName ::
    LabelledGraphProg ->             -- The initial program
    Name              ->             -- The name of the block to update info in
    Int               ->             -- The index of the InfoMap in the block
    Name              ->             -- The variable name we're interested in
    DataFlowResult LabelledGraphProg -- The resulting program
updateDataFlowInfoAtPointForName lProg blockName idx varName = do
    block <- lgpBlockLookup blockName lProg
    -- If we're looking at the first InfoMap in the block, there is no preceding
    -- assignment, so we're concerned with the end InfoMaps for the preceding
    -- blocks. If we're looking at a later assignment, we only need look at the
    -- previous InfoMap (there will be exactly one) and the assignment that
    -- comes after it. The following two branches handle each of those cases by
    -- retrieving the appropriate information and applying the appropriate
    -- rules.
    if idx == 0 then do
        prevInfoMaps   <- getPrevInfoMaps lProg blockName
        prevInfo       <- mapM (infoMapLookup varName) prevInfoMaps
        resultantInfo  <- return $ applyUpdateRulesBlockEntry prevInfo
        resultantBlock <- setInfoForNameAt block idx varName resultantInfo
        return $ M.insert blockName resultantBlock lProg
    else do
        assignment     <- getAssignAt block (idx - 1)
        prevInfo       <- getInfoForNameAt block (idx - 1) varName
        resultantInfo  <- return $ applyUpdateRulesInsideBlock varName
                                                            assignment prevInfo
        resultantBlock <- setInfoForNameAt block idx varName resultantInfo
        return $ M.insert blockName resultantBlock lProg

-- The following rules take a list of DataFlowInfo, containing the DataFlowInfo
-- after every block that can precede the block in question, for a single
-- variable, and computes the DataFlowInfo before the first assignment in the
-- block, for that variable.
applyUpdateRulesBlockEntry :: [DataFlowInfo] -> DataFlowInfo
applyUpdateRulesBlockEntry prevInfo
    -- If the value is unknowable on any of the paths into the statement,
    -- then the value is unknowable after the statement
    | any (== Unknowable) prevInfo = Unknowable

    -- If a value has not been assigned on every path into the statement,
    -- then it has not been assigned after the statement
    | all (== NotAssigned) prevInfo = NotAssigned

    -- If all paths into the statement are not unknowable, and ignoring
    -- paths where the value is not assigned (the previous rule guarantees
    -- that there is at least one path where it IS assigned), and those
    -- paths have a known values, but disagree on what the value is, then
    -- the value is unknowable after the statement
    | not (allSame (filter (/= NotAssigned) prevInfo)) = Unknowable

    -- The converse of the previous case - no unknowables, and all known
    -- values agree in value, then after the statement, the value is known
    -- to have the agreed upon value
    | allSame (filter (/= NotAssigned) prevInfo) =
        head (filter (/= NotAssigned) prevInfo)

-- The following rules take a variable name, an assignment and some DataFlowInfo
-- which is the status of the given variable name just before the given
-- assignment, and map this information to the status of the variable after the
-- assignment.
applyUpdateRulesInsideBlock :: Name -> Assignment -> DataFlowInfo ->
    DataFlowInfo
applyUpdateRulesInsideBlock varName assignment prevInfo
    -- If the statement is never reached, we can say for certain that the
    -- following statement will not be reached, regardless of the nature of the
    -- assignment statement. All patterns after this one can assume that the
    -- statement is executed.
    | prevInfo == NotAssigned = NotAssigned
    | otherwise           = case assignment of
        -- If the assignment from a constant, and the assignee is the variable
        -- in question, then after the statement, we know that the variable in
        -- question has the assigned value. This also applies for two constants
        -- combined with an operation.
        FromOne assignee (Lit x)            | assignee == varName -> Known x
        FromTwo assignee (Lit x) op (Lit y) | assignee == varName ->
            Known ((opToFunc op) x y)

        -- If there are any variables involved in the assignment, we naively say
        -- we can't know the value of the variable. We can do better than this
        -- with a more sophisticated implementation, however.
        FromOne assignee (Var _)           | assignee == varName -> Unknowable
        FromTwo assignee (Var _) _ (Lit _) | assignee == varName -> Unknowable
        FromTwo assignee (Lit _) _ (Var _) | assignee == varName -> Unknowable
        FromTwo assignee (Var _) _ (Var _) | assignee == varName -> Unknowable

        -- If the assignment isn't to the variable of interest, then we just
        -- return the input info, since the variable of interest won't change.
        FromOne assignee _     | assignee /= varName -> prevInfo
        FromTwo assignee _ _ _ | assignee /= varName -> prevInfo

{- DATA FLOW INFORMATION PROPAGATION RULES
info :: VarName -> Statement -> (In | Out) -> (_|_ | Const Int | ^|^)
iinfoinfoinfoinfoinfoinfonfo x stmt In =
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
