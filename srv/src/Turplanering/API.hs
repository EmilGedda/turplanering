{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Turplanering.API where

import Data.Morpheus.Types
import GHC.Generics
import qualified Turplanering.Map as Map

type GQLAPI m = RootResolver m () GQLQuery Undefined Undefined

newtype GQLQuery m = Query { getDetails :: Map.Box -> m Map.Details }
    deriving (Generic, GQLType)

gqlResolver :: Map.MonadStorage m => GQLAPI m
gqlResolver =
      RootResolver
        { queryResolver        = Query {getDetails = lift . Map.getDetails},
          mutationResolver     = Undefined,
          subscriptionResolver = Undefined
        }
