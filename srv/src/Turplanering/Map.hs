{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Turplanering.Map where

import           GHC.Generics
import           Data.Morpheus.Types
import           Control.Lens
import qualified Data.Text          as T
import Data.Morpheus.Kind

-- TODO: Improve type safety, and specify datum?
data Coordinate = Coord
  { _x :: Float,
    _y :: Float
  } deriving (Show, Generic)

instance GQLType Coordinate where
    type KIND Coordinate = INPUT

data Box = Box
  { _topLeft :: Coordinate,
    _bottomRight :: Coordinate
  } deriving (Show, Generic, GQLType)

class Monad m => MonadStorage m where
  getDetails :: Box -> m Details

data Details = Details
  { _trails :: [Trail]
  , _areas :: [Int]
  } deriving (Show, Generic, GQLType)

data TrailSection = TrailSection
  { _sectionName ::T.Text,
    _sectionDescription ::T.Text,
    _sectionPath :: T.Text -- TODO: Custom GeoJSON type
  } deriving (Show, Generic, GQLType)


data Trail = Trail
  { _trailName :: T.Text,
    _trailColor :: T.Text,
    _trailDescription :: T.Text,
    _trailSections :: [TrailSection]
  } deriving (Show, Generic, GQLType)


makeLenses ''Coordinate
makeLenses ''Box
makeLenses ''Details
makeLenses ''TrailSection
makeLenses ''Trail


point :: (Float, Float) -> Coordinate
point = uncurry Coord

box :: (Float, Float) -> (Float, Float) -> Box
box x y = Box (point x) (point y)
