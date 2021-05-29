{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Turplanering.DB.Trail where

import           Opaleye
import           Data.Profunctor.Product.TH
import qualified Data.Text as T

data DBTrail' a b c d = DBTrail
  { id          :: a
  , color       :: b
  , name        :: c
  , description :: d
  }

type DBTrail = DBTrail' Int T.Text T.Text T.Text
type DBTrailField = DBTrail'
                        (Field SqlInt4)
                        (Field SqlText)
                        (Field SqlText)
                        (Field SqlText)

type Table' t = Table t t

$(makeAdaptorAndInstance "pDBTrail" ''DBTrail')
