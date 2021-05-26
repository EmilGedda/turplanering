module Turplanering.Map where

import qualified Data.Text as T

-- TODO: Improve type safety, and specify datum?
data Coordinate = Coord
  { x :: Double,
    y :: Double
  } deriving Show

point :: (Double, Double) -> Coordinate
point = uncurry Coord

data Box = Box
  { topLeft :: Coordinate,
    bottomRight :: Coordinate
  } deriving Show

box :: (Double, Double) -> (Double, Double) -> Box
box x y = Box (point x) (point y)

class Monad m => MonadStorage m where
  getDetails :: Box -> m Details

data Details = Details
  { trails :: [Trail]
  , areas :: [Area]
  } deriving Show

data TrailSection = TrailSection
  { sectionName ::T.Text,
    sectionDescription ::T.Text,
    sectionPath :: [Coordinate]
  } deriving Show

data Trail = Trail
  { trailName :: T.Text,
    trailDescription :: T.Text,
    trailSections :: [TrailSection]
  } deriving Show

data Area = Area deriving Show
type Trails = [Trail]
