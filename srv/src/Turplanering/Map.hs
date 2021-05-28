{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
module Turplanering.Map where

import           GHC.Generics
import           Data.Morpheus.Types
import qualified Data.Text          as T
import Data.Morpheus.Kind

-- TODO: Improve type safety, and specify datum?
data Coordinate = Coord
  { x :: Float,
    y :: Float
  } deriving (Show, Generic)

instance GQLType Coordinate where
    type KIND Coordinate = INPUT

point :: (Float, Float) -> Coordinate
point = uncurry Coord

data Box = Box
  { topLeft :: Coordinate,
    bottomRight :: Coordinate
  } deriving (Show, Generic, GQLType)

box :: (Float, Float) -> (Float, Float) -> Box
box x y = Box (point x) (point y)

class Monad m => MonadStorage m where
  getDetails :: Box -> m Details

data Details = Details
  { trails :: [Trail]
  , areas :: [Int]
  } deriving (Show, Generic, GQLType)

data TrailSection = TrailSection
  { sectionName ::T.Text,
    sectionDescription ::T.Text,
    sectionPath :: T.Text -- TODO: Custom GeoJSON type
  } deriving (Show, Generic, GQLType)


data Trail = Trail
  { trailName :: T.Text,
    trailColor :: T.Text,
    trailDescription :: T.Text,
    trailSections :: [TrailSection]
  } deriving (Show, Generic, GQLType)
