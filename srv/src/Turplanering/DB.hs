{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module Turplanering.DB where

import           Control.Monad.Reader
import           Data.Maybe
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH
import           Database.PostgreSQL.Simple
import           Opaleye
import           Turplanering.Map
import           Turplanering.PostGIS
import Data.Ewkb
import Data.Geospatial
import Data.Hex
import Turplanering.Collections
import qualified Data.Map.Strict                as M
import qualified Data.Text                      as T
import Data.Aeson
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as B

newtype Config = DBConfig ConnectInfo

newtype Handle m a = DBHandle (ReaderT Connection m a)
    deriving (Functor, Applicative, Monad, MonadReader Connection, MonadIO)


data DBSections' a b c d e = DBSections
    { dbSectionId :: a
    , dbTrailOwnerID :: b
    , dbSectionName :: c
    , dbSectionDescription :: d
    , dbSectionPath :: e
    }

data DBTrail' a b c d = DBTrail
    { dbTrailID :: a,
      dbTrailColor :: b,
      dbTrailName :: c,
      dbTrailDescription :: d
    }

type TrailID = Int

type DBSections = DBSections' Int Int T.Text T.Text (Spatial Geography LineString)
type DBSectionsField = DBSections'
                        (Field SqlInt4)
                        (Field SqlInt4)
                        (Field SqlText)
                        (Field SqlText)
                        (Field (Spatial Geography LineString))

type DBTrail      = DBTrail' Int T.Text T.Text T.Text
type DBTrailField = DBTrail'
                        (Field SqlInt4)
                        (Field SqlText)
                        (Field SqlText)
                        (Field SqlText)

type Table' t = Table t t

class Monad m => MonadRDBMS m where
    select' :: Default FromFields a b => Select a -> m [b]

select :: forall b a m. (Default FromFields a b, MonadRDBMS m) => Select a -> m [b]
select = select'

$(makeAdaptorAndInstance "pDBSections" ''DBSections')
$(makeAdaptorAndInstance "pDBTrail" ''DBTrail')

trailsTable :: Table' DBTrailField
trailsTable = table "trails" $
                pDBTrail DBTrail
                    { dbTrailID          = tableField "id"
                    , dbTrailColor       = tableField "color"
                    , dbTrailName        = tableField "name"
                    , dbTrailDescription = tableField "description" }


sectionsTable :: Table' DBSectionsField
sectionsTable = table "trail_sections" $
                pDBSections DBSections
                    { dbSectionId          = tableField "id"
                    , dbTrailOwnerID       = tableField "trail_id"
                    , dbSectionName        = tableField "name"
                    , dbSectionDescription = tableField "description"
                    , dbSectionPath        = tableField "geog" }


sectionsInside :: Box -> Select DBSectionsField
sectionsInside area = do
    s <- selectTable sectionsTable
    where_ $ dbSectionPath s &&: toGeography (makeEnvelope area)
    return s


trailsFrom :: [TrailID] -> Select DBTrailField
trailsFrom ids = do
    t <- selectTable trailsTable
    where_ $ map sqlInt4 ids `in_` dbTrailID t
    return t


runHandle :: MonadIO m => Config -> Handle m a -> m a
runHandle (DBConfig cfg) (DBHandle r) = runReaderT r =<< liftIO (connect cfg)


printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . fromMaybe "No query" . showSql

trailFromDB :: DBTrail -> Trail
trailFromDB (DBTrail _ c n d) = Trail n c d []

wkbToGeoJSON :: Spatial a b -> GeospatialGeometry
wkbToGeoJSON (SpatialObject (WKB bs))
    = let Right x = parseHexByteString (Hex bs) in x

sectionFromDB :: DBSections -> TrailSection
sectionFromDB (DBSections _ _ n d spatial)
  = TrailSection n d (decodeUtf8 . B.toStrict . encode $ wkbToGeoJSON spatial)

insertSection :: TrailSection -> Trail -> Trail
insertSection s t = t { trailSections = s:trailSections t }

fetchDetails :: MonadRDBMS m => Box -> m Details
fetchDetails bbox = do
        sections  <- select @DBSections (sectionsInside bbox)
        trailData <- select . trailsFrom $ map dbTrailOwnerID sections
        let trailTbl = trailFromDB <$> bucketOn dbTrailID trailData
            insert   = M.adjust <$> insertSection . sectionFromDB <*> dbTrailOwnerID
            trails'  = foldr insert trailTbl sections
        return $ Details (M.elems trails') []

instance MonadIO m => MonadStorage (Handle m) where
    getDetails = DBHandle . fetchDetails

instance MonadIO m => MonadRDBMS (ReaderT Connection m) where
    select' s = ReaderT $ \c -> liftIO (runSelect c s)

instance MonadIO m => MonadRDBMS (Handle m) where
    select' = DBHandle . select'

devConfig :: Config
devConfig = DBConfig (ConnectInfo "localhost" 5432 "user" "password" "turplanering")
