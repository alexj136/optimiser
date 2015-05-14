module Optimiser where

data ConstantPropInfo
    = NotExecuted
    | Known Int
    | Unknown
    deriving (Eq, Ord)

-- Generate constant propagation info for a given program
genConstantPropInfo :: GraphProg -> M.Map Name (Block, [ConstantPropInfo])
genConstantPropInfo prog = error "Not yet implemented"

-- Generate constant propagation info for a single block in a program, for a
-- single variable
genCPIBlock ::
    GraphProg   ->
    Name        ->  -- The name of the block to generate info for
    Name        ->  -- The name of the variable to analyse
    (Block, [ConstantPropInfo])
genCPIBlock prog blockName =
    let block   = prog M.! blockName
        assigns = assignments block
        adj     = adjacency block
    in
