{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Turplanering.DB.Trail where

import           Opaleye
import           Data.GenValidity
import           Data.Profunctor.Product.TH
import           GHC.Generics
import qualified Data.Text as T

data DBTrail' a b c d = DBTrail
  { id          :: a
  , color       :: b
  , name        :: c
  , description :: d
  } deriving (Show, Eq, Generic, Validity)

instance (GenValid a, GenValid b, GenValid c, GenValid d)
         => GenValid (DBTrail' a b c d) where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally

type DBTrail = DBTrail' Int T.Text T.Text T.Text
type DBTrailField = DBTrail'
                        (Field SqlInt4)
                        (Field SqlText)
                        (Field SqlText)
                        (Field SqlText)

type Table' t = Table t t

$(makeAdaptorAndInstance "pDBTrail" ''DBTrail')
