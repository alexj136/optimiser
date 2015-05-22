module Util (notImplemented, update) where

import qualified Data.Sequence as Sq
import qualified Data.Set      as St
import Data.Foldable (toList)

notImplemented :: a
notImplemented = error "Not yet implemented"

-- Replace the element at a given list+index with the given element
update :: Int -> a -> [a] -> [a]
update idx elem = toList . (Sq.update idx elem) . Sq.fromList

-- Determine if all elements in a list are equal
allSame :: Ord a => [a] -> Bool
allSame = (1 >=) . St.size . St.fromList
