{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Turplanering.Map where

import           Control.Arrow
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Geospatial
import           GHC.Generics
import           Optics
import           Servant.API
import qualified Data.Text          as T

class Monad m => MonadStorage m where
  getDetails :: Box -> m Details

data Box = Box
  { topLeft :: PointXY
  , bottomRight :: PointXY
  } deriving (Show, Generic)

boxParser :: Parser Box
boxParser = mkBox <$> point <*> (char ',' *> point)
    where point = (,) <$> double <*> (char ',' *> double)

instance FromHttpApiData Box where
    parseUrlPiece = left T.pack . parseOnly boxParser

data Details = Details
  { trails :: [Trail]
  , areas :: [Int]
  } deriving (Show, Generic, ToJSON)

data Section = Section
  { name ::T.Text
  , description :: T.Text
  , path :: GeospatialGeometry
  } deriving (Show, Generic, ToJSON)


data Trail = Trail
  { name :: T.Text
  , color :: T.Text
  , description :: T.Text
  , sections :: [Section]
  } deriving (Show, Generic, ToJSON)


mkCoord :: (Double, Double) -> PointXY
mkCoord = uncurry PointXY

mkBox :: (Double, Double) -> (Double, Double) -> Box
mkBox a b = Box (mkCoord a) (mkCoord b)

makeFieldLabelsWith noPrefixFieldLabels ''Trail
makeFieldLabelsWith noPrefixFieldLabels ''Section
makeFieldLabelsWith noPrefixFieldLabels ''Details
makeFieldLabelsWith noPrefixFieldLabels ''Box
