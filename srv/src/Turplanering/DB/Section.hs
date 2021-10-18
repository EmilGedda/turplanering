{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Turplanering.DB.Section (
    DBSections' (..),
    TrailID,
    DBSections,
    DBSectionsField,
    pDBSections,
) where

import           Data.Function              (on)
import           Data.GenValidity
import           Data.Profunctor.Product.TH
import           GHC.Generics
import           Opaleye
import           Optics
import           Prelude                    hiding (id)
import qualified Data.Text                  as T

import Turplanering.PostGIS


data DBSections' a b c d e = DBSections
    { id :: a
    , trailId :: b
    , name :: c
    , description :: d
    , path :: e
    }
    deriving (Show, Generic, Validity)


instance
    (GenValid a, GenValid b, GenValid c, GenValid d, GenValid e) =>
    GenValid (DBSections' a b c d e)
    where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally


instance Eq a => Eq (DBSections' a b c d e) where
    (==) = (==) `on` id


type TrailID = Int


type DBSections =
    DBSections'
        Int
        Int
        T.Text
        T.Text
        (Spatial Geography LineString)


type DBSectionsField =
    DBSections'
        (Field SqlInt4)
        (Field SqlInt4)
        (Field SqlText)
        (Field SqlText)
        (Field (Spatial Geography LineString))


$(makeAdaptorAndInstanceInferrable' ''DBSections')
makeFieldLabelsWith noPrefixFieldLabels ''DBSections'
