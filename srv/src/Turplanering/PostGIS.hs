{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}

module Turplanering.PostGIS where

import           Control.Exception
import           Data.Ewkb
import           Data.GenValidity
import           Data.GenValidity.ByteString          ()
import           Data.Geospatial
import           Data.Hex
import           Data.Proxy
import           Data.Validity.ByteString             ()
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           GHC.TypeLits
import           Opaleye
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as C
import qualified Database.PostgreSQL.Simple.FromField as PQ
import qualified Opaleye.Internal.Column              as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import Turplanering.Map


newtype WKB = WKB B.ByteString
    deriving (PQ.FromField) via (Binary B.ByteString)
    deriving stock (Generic)
    deriving anyclass (Validity)


instance GenValid WKB where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally


instance Show WKB where
    show (WKB bs) = "WKB " <> show (parseHexByteString $ Hex bs)


type Geometry = "geometry"


type Geography = "geography"


data LineString deriving (Show)


data MultiLineString deriving (Show)


data Polygon deriving (Show)


data Point deriving (Show)


class Show a => SpatialType a


instance SpatialType LineString


instance SpatialType MultiLineString


instance SpatialType Polygon


instance SpatialType Point


data Spatial (geo :: Symbol) t where
    SpatialObject :: WKB -> Spatial geo t
    deriving (Show, Generic, Validity)


instance GenValid (Spatial geo t) where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally


throwErr ::
    Exception err =>
    String ->
    PQ.Field ->
    (String -> Maybe PQ.Oid -> String -> String -> String -> err) ->
    String ->
    PQ.Conversion a
throwErr sym f mkErr msg = do
    typnam <- PQ.typename f
    PQ.conversionError $
        mkErr
            (C.unpack typnam)
            (PQ.tableOid f)
            (maybe "" C.unpack $ PQ.name f)
            ("expected: " ++ sym)
            msg


instance KnownSymbol a => PQ.FromField (Spatial a t) where
    fromField field m = do
        dbType <- PQ.typename field
        let expected = symbolVal (Proxy :: Proxy a)
            err = throwErr expected field
        if
                | dbType /= C.pack expected -> err Incompatible (show dbType)
                | Nothing <- m -> err UnexpectedNull "no data"
                | Just bs <- m -> return . SpatialObject $ WKB bs


instance KnownSymbol a => DefaultFromField (Spatial a b) (Spatial a b) where
    defaultFromField = fromPGSFromField


makeEnvelope :: Box -> Field (Spatial Geometry Polygon)
makeEnvelope (Box (PointXY a b) (PointXY x y)) =
    Opaleye.Column . HPQ.FunExpr "ST_MakeEnvelope" $
        map (HPQ.ConstExpr . HPQ.DoubleLit . realToFrac) [a, b, x, y]


(&&:) :: Field (Spatial a b) -> Field (Spatial a c) -> Field SqlBool
(&&:) = Opaleye.binOp (HPQ.:&&)


toGeography :: Field (Spatial Geometry a) -> Field (Spatial Geography a)
toGeography = unsafeCast "geography"
