{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Turplanering.DB where


import           Control.Arrow
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH
import           Database.PostgreSQL.Simple
import           Opaleye
import           Turplanering.Map
import           Turplanering.PostGIS
import qualified Data.Map.Strict                as M
import qualified Data.Text                      as T


newtype Config = DBConfig ConnectInfo

newtype Handle m a = DBHandle (ReaderT Connection m a)
  deriving (Functor, Applicative, Monad, MonadReader Connection)

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

$(makeAdaptorAndInstance "pDBSections" ''DBSections')
$(makeAdaptorAndInstance "pDBTrail" ''DBTrail')

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

type Selector a b = forall m. (MonadIO m, MonadReader Connection m) => Select a -> m [b]

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

runDB :: MonadIO m => Config -> Handle m a -> m a
runDB (DBConfig cfg) (DBHandle r) = runReaderT r =<< liftIO (connect cfg)

instance MonadIO m => MonadIO (Handle m) where
    liftIO = DBHandle . liftIO

instance MonadIO m => MonadStorage (Handle m) where
    getDetails = DBHandle . fetchDetails

printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . fromMaybe "No query" . showSql

select :: forall b a m. (Default FromFields a b, MonadIO m, MonadReader Connection m) => Select a -> m [b]
select s = liftIO . flip runSelect s =<< ask

fetchDetails :: (MonadIO m, MonadReader Connection m) => Box -> m Details
fetchDetails bbox = do
        sections' <- select @DBSections (sectionsInside bbox)
        trails'   <- select @DBTrail . trailsFrom $ map dbTrailOwnerID sections'
        let trailTable = M.fromList $ map (dbTrailID &&& fromDB) trails'
            fromDB (DBTrail _ _ n d) = Trail n d []
            insert (DBSections _ _ n d _) t = t { trailSections = TrailSection n d []:trailSections t }
            complete = foldr (\s -> M.adjust (insert s) (dbTrailOwnerID s)) trailTable sections'
        return $ Details (M.elems complete) []

devConfig :: Config
devConfig = DBConfig (ConnectInfo "localhost" 5432 "user" "password" "turplanering")
