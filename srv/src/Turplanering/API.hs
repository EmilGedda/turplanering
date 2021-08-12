{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Turplanering.API where

import           Control.Monad.Reader
import           Database.PostgreSQL.Simple
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           Turplanering.Map
import           Turplanering.DB
import qualified Turplanering.Config as Config

newtype Routes route = Routes
    { _get :: route :- "trails" :> Capture "area" Box :> Get '[JSON] Details }
    deriving (Generic)

data AppContext = AppContext
    { config :: Config.App
    , dbConn :: Connection
    }

newtype ContextM a = ContextM
    { runContext :: ReaderT AppContext Handler a }
    deriving (Functor, Applicative, Monad, MonadReader AppContext, MonadIO)

instance MonadStorage ContextM where
    getDetails box = withConnection (getDetails box) =<< asks dbConn

routes :: MonadStorage m => Routes (AsServerT m)
routes = Routes
    { _get = getDetails }

runApp :: AppContext -> ContextM a -> Handler a
runApp cfg = flip runReaderT cfg . runContext

api :: AppContext -> Application
api cfg = genericServeT (runApp cfg) routes
