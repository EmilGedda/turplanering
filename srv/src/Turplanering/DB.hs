{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Turplanering.DB where

import           Control.Monad.Reader
import           Control.Lens
import           Data.Ewkb
import           Data.Geospatial
import           Data.Hex
import           Data.Maybe
import           Data.Profunctor.Product.Default
import           Database.PostgreSQL.Simple
import           Opaleye
import           Turplanering.Collections
import           Turplanering.Map
import           Turplanering.PostGIS
import           Turplanering.DB.Types
import qualified Turplanering.DB.Section        as Section
import qualified Turplanering.DB.Trail          as Trail
import qualified Data.Map.Strict                as M
import qualified Turplanering.Config as Config

newtype Handle m a = Handle (ReaderT Connection m a)
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader Connection, MonadRDBMS)

getConnectionInfo :: Config.DB -> ConnectInfo
getConnectionInfo (Config.DB addr port user pass db)
    = ConnectInfo addr (fromIntegral port) user pass db

class Monad m => MonadRDBMS m where
    select :: Default FromFields a b => Select a -> m [b]

trailsTable :: Table' DBTrailField
trailsTable = table "trails"
            . pDBTrail
            $ DBTrail
                    (tableField "id")
                    (tableField "color")
                    (tableField "name")
                    (tableField "description")


sectionsTable :: Table' DBSectionsField
sectionsTable = table "trail_sections"
              . pDBSections
              $ DBSections
                    (tableField "id")
                    (tableField "trail_id")
                    (tableField "name")
                    (tableField "description")
                    (tableField "geog")


sectionsInside :: Box -> Select DBSectionsField
sectionsInside area = do
    s <- selectTable sectionsTable
    where_ $ Section.path s &&: toGeography (makeEnvelope area)
    return s


trailsFrom :: [TrailID] -> Select DBTrailField
trailsFrom ids = do
    t <- selectTable trailsTable
    where_ $ map sqlInt4 ids `in_` Trail.id t
    return t

withConnection :: Handle m a -> Connection -> m a
withConnection (Handle hndl) = runReaderT hndl


printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . fromMaybe "No query" . showSql


trailFromDB :: DBTrail -> Trail
trailFromDB (DBTrail _ c n d) = Trail n c d []


wkbToGeoJSON :: Spatial a b -> GeospatialGeometry
wkbToGeoJSON (SpatialObject (WKB bs))
    = let Right x = parseHexByteString (Hex bs) in x


sectionFromDB :: DBSections -> TrailSection
sectionFromDB (DBSections _ _ n d spatial)
  = TrailSection n d (wkbToGeoJSON spatial)


insertSection :: TrailSection -> Trail -> Trail
insertSection s = over _trailSections (s:)

buildTrails :: [DBTrail] -> [DBSections] -> [Trail]
buildTrails trailData = M.elems . foldr insert trailTbl
    where trailTbl = trailFromDB <$> bucketOn Trail.id trailData
          insert   = M.adjust <$> insertSection . sectionFromDB
                              <*> Section.trailId


fetchDetails :: MonadRDBMS m => Box -> m Details
fetchDetails bbox = do
        sections  <- select (sectionsInside bbox)
        trailData <- select . trailsFrom $ map Section.trailId sections
        return $ Details (buildTrails trailData sections) []

instance MonadIO m => MonadStorage (Handle m) where
    getDetails = fetchDetails

instance MonadIO m => MonadRDBMS (ReaderT Connection m) where
    select query = ReaderT $ \conn -> liftIO (runSelect conn query)
