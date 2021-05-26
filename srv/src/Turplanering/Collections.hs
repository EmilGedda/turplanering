module Turplanering.Collections where

import qualified Data.Map.Strict as M

bucketOn :: (a -> k) -> [a] -> M.Map k [a]
bucketOn f xs = M.empty

mergeBucket :: (a -> k) -> [a] -> M.Map k [b] -> (a -> [b] -> c) -> [c]
mergeBucket = undefined

