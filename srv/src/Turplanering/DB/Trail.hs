{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Turplanering.DB.Trail (
    DBTrail' (..),
    DBTrail,
    DBTrailField,
    Table',
    pDBTrail,
) where

import Data.GenValidity
import Data.Profunctor.Product.TH
import qualified Data.Text as T
import GHC.Generics
import Opaleye
import Optics

data DBTrail' a b c d = DBTrail
    { id :: a
    , color :: b
    , name :: c
    , description :: d
    }
    deriving (Show, Eq, Generic, Validity)

instance
    (GenValid a, GenValid b, GenValid c, GenValid d) =>
    GenValid (DBTrail' a b c d)
    where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally

type DBTrail = DBTrail' Int T.Text T.Text T.Text

type DBTrailField =
    DBTrail'
        (Field SqlInt4)
        (Field SqlText)
        (Field SqlText)
        (Field SqlText)

type Table' t = Table t t

$(makeAdaptorAndInstanceInferrable' ''DBTrail')
makeFieldLabelsWith noPrefixFieldLabels ''DBTrail'
