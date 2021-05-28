{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Turplanering.API where

import Data.Morpheus.Types
import GHC.Generics
import Turplanering.Map

type GQLAPI m = RootResolver m () GQLQuery Undefined Undefined

newtype GQLQuery m = Query { details :: Box -> m Details }
    deriving (Generic, GQLType)

gqlResolver :: MonadStorage m => GQLAPI m
gqlResolver =
      RootResolver
        { queryResolver        = Query {details = lift . getDetails},
          mutationResolver     = Undefined,
          subscriptionResolver = Undefined
        }
