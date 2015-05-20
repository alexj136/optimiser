module Util (notImplemented, update) where

import qualified Data.Sequence as Sq
import Data.Foldable (toList)

notImplemented :: a
notImplemented = error "Not yet implemented"

-- Replace the element at a given list+index with the given element
update :: Int -> a -> [a] -> [a]
update idx elem = toList . (Sq.update idx elem) . Sq.fromList
