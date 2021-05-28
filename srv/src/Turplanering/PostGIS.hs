{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Turplanering.PostGIS where

import           Control.Exception
import           Data.Proxy
import           Database.PostgreSQL.Simple
import           GHC.TypeLits
import           Opaleye
import           Turplanering.Map
import qualified Data.ByteString                        as B
import qualified Data.ByteString.Char8                  as B8
import qualified Database.PostgreSQL.Simple.FromField   as PQ
import qualified Opaleye.Internal.Column                as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery   as HPQ

-- TODO: Parse this properly
newtype WKB = WKB B.ByteString
    deriving PQ.FromField via (Binary B.ByteString)

type Geometry  = "geometry"
type Geography = "geography"

data LineString
data MultiLineString
data Polygon
data Point

class SpacialType a

instance SpacialType LineString
instance SpacialType MultiLineString
instance SpacialType Polygon
instance SpacialType Point

newtype Spatial (geo :: Symbol) t where
    SpatialObject :: WKB -> Spatial geo t

throwErr :: Exception err
            => String
            -> PQ.Field
            -> (String -> Maybe PQ.Oid -> String -> String -> String -> err)
            -> String -> PQ.Conversion a
throwErr sym f mkErr msg = do
  typnam <- PQ.typename f
  PQ.conversionError
    $ mkErr (B8.unpack typnam)
            (PQ.tableOid f)
            (maybe "" B8.unpack $ PQ.name f)
            ("expected: " ++ sym)
            msg

instance KnownSymbol a => PQ.FromField (Spatial a t) where
    fromField field m = do
        dbType <- PQ.typename field
        let expected = symbolVal (Proxy :: Proxy a)
            err = throwErr expected field
        if | dbType /= B8.pack expected -> err Incompatible (show dbType)
           | Nothing <- m               -> err UnexpectedNull "no data"
           | Just bs <- m               -> return . SpatialObject $ WKB bs

instance KnownSymbol a => DefaultFromField (Spatial a b) (Spatial a b) where
    defaultFromField = fromPGSFromField

makeEnvelope :: Box -> Field (Spatial Geometry Polygon)
makeEnvelope (Box (Coord a b) (Coord x y))
  = C.Column . HPQ.FunExpr "ST_MakeEnvelope"
  $ map (HPQ.ConstExpr . HPQ.DoubleLit . realToFrac) [a, b, x, y]

(&&:) :: Field (Spatial a b) -> Field (Spatial a c) -> Field SqlBool
(&&:) = C.binOp (HPQ.:&&)

toGeography :: Field (Spatial Geometry a) -> Field (Spatial Geography a)
toGeography = unsafeCast "geography"
