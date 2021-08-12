{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Turplanering.Map where

import           GHC.Generics
import           Control.Lens
import           Data.Geospatial
import           Data.Attoparsec.Text
import           Servant.API
import qualified Data.Text          as T
import Control.Arrow
import Data.Aeson

data Box = Box
  { topLeft :: PointXY
  , bottomRight :: PointXY
  } deriving (Show, Generic)

class Monad m => MonadStorage m where
  getDetails :: Box -> m Details

data Details = Details
  { trails :: [Trail]
  , areas :: [Int]
  } deriving (Show, Generic, ToJSON)

data TrailSection = TrailSection
  { sectionName ::T.Text
  , sectionDescription ::T.Text
  , sectionPath :: GeospatialGeometry
  } deriving (Show, Generic, ToJSON)


data Trail = Trail
  { trailName :: T.Text
  , trailColor :: T.Text
  , trailDescription :: T.Text
  , trailSections :: [TrailSection]
  } deriving (Show, Generic, ToJSON)

boxParser :: Parser Box
boxParser = mkBox <$> point <*> (char ',' *> point)
    where point = (,) <$> double <*> (char ',' *> double)

instance FromHttpApiData Box where
    parseUrlPiece = left T.pack . parseOnly boxParser

mkCoord :: (Double, Double) -> PointXY
mkCoord = uncurry PointXY

mkBox :: (Double, Double) -> (Double, Double) -> Box
mkBox a b = Box (mkCoord a) (mkCoord b)


makeClassy_ ''Box
makeClassy_ ''Details
makeClassy_ ''TrailSection
makeClassy_ ''Trail
