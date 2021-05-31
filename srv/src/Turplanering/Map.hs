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
  { x :: Float,
    y :: Float
  } deriving (Show, Generic)

instance GQLType Coordinate where
    type KIND Coordinate = INPUT

data Box = Box
  { topLeft :: Coordinate,
    bottomRight :: Coordinate
  } deriving (Show, Generic, GQLType)

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



mkCoord :: (Float, Float) -> Coordinate
mkCoord = uncurry Coord

mkBox :: (Float, Float) -> (Float, Float) -> Box
mkBox x y = Box (mkCoord x) (mkCoord y)


makeClassy_ ''Coordinate
makeClassy_ ''Box
makeClassy_ ''Details
makeClassy_ ''TrailSection
makeClassy_ ''Trail
