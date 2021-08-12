{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Turplanering.API where

import           Control.Monad.Reader
import           Database.PostgreSQL.Simple
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           Turplanering.Map
import           Turplanering.DB
import qualified Turplanering.Config as Config
import Turplanering.Logger
import Network.Wai
import qualified Data.Text.Encoding as T

newtype Routes route = Routes
    { _get :: route :- "trails" :> Capture "area" Box :> Get '[JSON] Details }
    deriving (Generic)

data AppContext = AppContext
    { config :: Config.App
    , dbConn :: Connection
    }

newtype ContextM a = ContextM
     -- drop Handler for IO and use MonadThrow and MonadCatch
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


requestLogger :: Middleware
requestLogger app req resp = do
        logger Debug "serving http request"
            & field "method" (T.decodeUtf8 $ requestMethod req)
            . field "path"   (T.decodeUtf8 $ rawPathInfo   req)
            . field "from"   (show         $ remoteHost    req)
        app req resp
    where logger = consoleLogger Trace "http"
