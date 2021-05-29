{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Turplanering.DB.Section where

import          Opaleye
import          Turplanering.PostGIS
import           Data.Profunctor.Product.TH
import qualified Data.Text as T

data DBSections' a b c d e = DBSections
    { id          :: a
    , trailId     :: b
    , name        :: c
    , description :: d
    , path        :: e
    }

type TrailID = Int

type DBSections = DBSections'
                        Int
                        Int
                        T.Text
                        T.Text
                        (Spatial Geography LineString)

type DBSectionsField = DBSections'
                        (Field SqlInt4)
                        (Field SqlInt4)
                        (Field SqlText)
                        (Field SqlText)
                        (Field (Spatial Geography LineString))

$(makeAdaptorAndInstance "pDBSections" ''DBSections')
