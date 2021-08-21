{-# LANGUAGE OverloadedLabels #-}

module Turplanering.DB (
    module Turplanering.DB.Section,
    module Turplanering.DB.Trail,
    Handle,
    MonadRDBMS,
    getConnectionInfo,
    withConnection,
    buildTrails,
) where

import           Control.Monad.Reader
import           Data.Ewkb
import           Data.Geospatial
import           Data.Hex
import           Data.Profunctor.Product.Default
import           Database.PostgreSQL.Simple
import           Opaleye
import           Optics
import qualified Data.Map.Strict                 as M

import           Turplanering.Collections
import           Turplanering.DB.Section
import           Turplanering.DB.Trail
import           Turplanering.Map
import           Turplanering.PostGIS
import qualified Turplanering.Config      as Config
import qualified Turplanering.DB.Section  as Section
import qualified Turplanering.DB.Trail    as Trail

newtype Handle m a = Handle (ReaderT Connection m a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Connection
        , MonadRDBMS
        )

getConnectionInfo :: Config.DB -> ConnectInfo
getConnectionInfo (Config.DB addr port user pass db) =
    ConnectInfo addr (fromIntegral port) user pass db

class Monad m => MonadRDBMS m where
    select :: Default FromFields a b => Select a -> m [b]

trailsTable :: Table' DBTrailField
trailsTable =
    table "trails"
        . pDBTrail
        $ DBTrail
            (tableField "id")
            (tableField "color")
            (tableField "name")
            (tableField "description")

sectionsTable :: Table' DBSectionsField
sectionsTable =
    table "trail_sections"
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

trailFromDB :: DBTrail -> Trail
trailFromDB (DBTrail _ c n d) = Trail n c d []

wkbToGeoJSON :: Spatial a b -> GeospatialGeometry
wkbToGeoJSON (SpatialObject (WKB bs)) =
    let Right x = parseHexByteString (Hex bs) in x

sectionFromDB :: DBSections -> Section
sectionFromDB (DBSections _ _ n d spatial) =
    Section n d (wkbToGeoJSON spatial)

insertSection :: Section -> Trail -> Trail
insertSection s = over #sections (s :)

buildTrails :: [DBTrail] -> [DBSections] -> [Trail]
buildTrails trailData = M.elems . foldr insert trailTbl
    where
        trailTbl = trailFromDB <$> bucketOn Trail.id trailData
        insert =
            M.adjust <$> insertSection . sectionFromDB
                <*> Section.id

fetchDetails :: MonadRDBMS m => Box -> m Details
fetchDetails bbox = do
    sections <- select (sectionsInside bbox)
    trailData <- select . trailsFrom $ map trailId sections
    return $ Details (buildTrails trailData sections) []

instance MonadIO m => MonadStorage (Handle m) where
    getDetails = fetchDetails

instance MonadIO m => MonadRDBMS (ReaderT Connection m) where
    select query = liftIO . flip runSelect query =<< ask
