{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Turplanering.API where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Word
import           Database.PostgreSQL.Simple
import           GHC.IO
import           Network.Wai
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           System.Random
import           Turplanering.DB
import           Turplanering.Logger
import           Turplanering.Map
import qualified Data.Text.Encoding as T
import qualified Data.Vault.Lazy as V
import qualified Turplanering.Config as Config

newtype Routes route = Routes
    { _get :: route :- "trails" :> Capture "area" Box :> Get '[JSON] Details }
    deriving (Generic)

data AppContext = AppContext
    { config :: Config.App
    , dbConn :: Connection
    }

data IDGen = RandomID StdGen
           | Sequential Int

data RequestContext = RequestContext
    { ctx :: AppContext
    , requestID :: RequestID
    }

newtype RequestID = RequestID { getID :: Word16 }
    deriving (ToJSON)

{-# NOINLINE requestIDKey #-}
requestIDKey :: V.Key RequestID
requestIDKey = unsafePerformIO V.newKey


newtype ContextM a = ContextM
     -- drop Handler for IO and use MonadThrow and MonadCatch
    { runContext :: ReaderT RequestContext Handler a }
    deriving (Functor, Applicative, Monad, MonadReader RequestContext, MonadIO)

instance MonadStorage ContextM where
    getDetails box = withConnection (getDetails box) =<< asks (dbConn . ctx)


routes :: MonadStorage m => Routes (AsServerT m)
routes = Routes
    { _get = getDetails }

runApp :: AppContext -> RequestID -> ContextM a -> Handler a
runApp cfg id handler = runReaderT (runContext handler) (RequestContext cfg id)

api :: AppContext -> RequestID -> Application
api cfg id = genericServeT (runApp cfg id) routes

getRequestID :: Request -> Maybe RequestID
getRequestID = V.lookup requestIDKey . vault

requestLogger :: Middleware
requestLogger app req resp = do
        logger Debug "serving http request"
            & field      "method" (T.decodeUtf8 $ requestMethod req)
            . field      "path"   (T.decodeUtf8 $ rawPathInfo   req)
            . field      "from"   (show         $ remoteHost    req)
            . fieldMaybe "reqID"  (getRequestID req)
        app req resp
    where logger = consoleLogger Trace "http"

withRequestID :: (RequestID -> Application) -> Application
withRequestID app req resp = do
        number <- randomIO
        let id'    = RequestID number
            vault' = V.insert requestIDKey id' $ vault req
            req'   = req { vault = vault' }
        app id' req' resp
