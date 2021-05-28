module Turplanering.Collections where

import qualified Data.Map.Strict as M
import Control.Arrow

bucketOn :: Ord k => (a -> k) -> [a] -> M.Map k a
bucketOn f = M.fromList . map (f &&& id)

mergeBucket :: (a -> k) -> [a] -> M.Map k [b] -> (a -> [b] -> c) -> [c]
mergeBucket = undefined

