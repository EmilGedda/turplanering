module Turplanering.Collections where

import           Control.Arrow
import           Data.Function
import           Data.List
import qualified Data.Map.Strict as M

bucketOn :: Ord k => (a -> k) -> [a] -> M.Map k a
bucketOn f = M.fromList . map (f &&& id)

nubSort :: Ord a => [a] -> [a]
nubSort = nubSortBy compare

nubSortOn :: Ord k => (a -> k) -> [a] -> [a]
nubSortOn f = nubSortBy (compare `on` f)

nubSortBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortBy cmp = uniq . sortBy cmp
    where uniq (x:x':xs) | cmp x x' == EQ = uniq (x:xs)
          uniq (x:xs) = x:uniq xs
          uniq [] = []

