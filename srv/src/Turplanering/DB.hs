{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Turplanering.DB where

import           Control.Monad.Reader
import           Control.Lens
import           Data.Aeson
import           Data.Ewkb
import           Data.Geospatial
import           Data.Hex
import           Data.Maybe
import           Data.Profunctor.Product.Default
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple
import           Opaleye
import           Turplanering.Collections
import           Turplanering.Map
import           Turplanering.PostGIS
import           Turplanering.DB.Types
import qualified Turplanering.DB.Section        as Section
import qualified Turplanering.DB.Trail          as Trail
import qualified Data.ByteString.Lazy           as B
import qualified Data.Map.Strict                as M

newtype Config = DBConfig ConnectInfo

newtype Handle m a = DBHandle (ReaderT Connection m a)
    deriving (Functor, Applicative, Monad, MonadReader Connection, MonadIO)


class Monad m => MonadRDBMS m where
    select' :: Default FromFields a b => Select a -> m [b]

select :: forall b a m. (Default FromFields a b, MonadRDBMS m)
       => Select a -> m [b]
select = select'


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


connectDB :: MonadIO m => Config -> Handle m a -> m a
connectDB (DBConfig cfg) (DBHandle r) = runReaderT r =<< liftIO (connect cfg)


withConnection :: Connection -> Handle m a -> m a
withConnection conn (DBHandle hndl) = runReaderT hndl conn


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
insertSection s = over trailSections (s:)


fetchDetails :: MonadRDBMS m => Box -> m Details
fetchDetails bbox = do
        sections  <- select (sectionsInside bbox)
        trailData <- select . trailsFrom $ map Section.trailId sections
        let insert = M.adjust <$> insertSection . sectionFromDB
                              <*> Section.trailId
            trailTbl = trailFromDB <$> bucketOn Trail.id trailData
            trails'  = foldr insert trailTbl sections
        return $ Details (M.elems trails') []


devConfig :: Config
devConfig = DBConfig (ConnectInfo "localhost" 5432 "user" "password" "turplanering")

instance MonadIO m => MonadStorage (Handle m) where
    getDetails = fetchDetails

instance MonadIO m => MonadRDBMS (ReaderT Connection m) where
    select' s = ReaderT $ \c -> liftIO (runSelect c s)

instance MonadIO m => MonadRDBMS (Handle m) where
    select' = DBHandle . select'
