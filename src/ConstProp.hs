module ConstProp where

import qualified Data.Map as M
import Util
import Syntax
import DataFlow

constProp :: LabelledGraphProg -> GraphProg
constProp = M.map (\(LabelledBlock (asmtsWithInfo, _, adj)) ->
    Block (map snd ((map constPropAssignment) asmtsWithInfo), adj))

constPropAssignment :: (InfoMap, Assignment) -> (InfoMap, Assignment)
constPropAssignment (infoMap, assignment) = (infoMap, newAssignment)
    where
    newAssignment = case assignment of
        FromOne assignee (Lit _)           -> assignment
        FromTwo assignee (Lit _) _ (Lit _) -> assignment
        FromOne assignee (Var x)            | isKnown (infoMap M.! x) ->
            FromOne assignee (Lit (getKnownVal (infoMap M.! x)))
        FromTwo assignee (Lit i) op (Var x) | isKnown (infoMap M.! x) ->
            FromTwo assignee (Lit i) op (Lit (getKnownVal (infoMap M.! x)))
        FromTwo assignee (Var x) op (Lit i) | isKnown (infoMap M.! x) ->
            FromTwo assignee (Lit (getKnownVal (infoMap M.! x))) op (Lit i)
        FromTwo assignee (Var x) op (Var y)
            | isKnown (infoMap M.! x) && isKnown (infoMap M.! y) ->
                FromTwo assignee (Lit (getKnownVal (infoMap M.! x))) op
                                            (Lit (getKnownVal (infoMap M.! y)))
            | isKnown (infoMap M.! x)                            ->
                FromTwo assignee (Lit (getKnownVal (infoMap M.! x))) op (Var y)
            |                            isKnown (infoMap M.! y) ->
                FromTwo assignee (Var x) op (Lit (getKnownVal (infoMap M.! y)))
            | otherwise                                          -> assignment
