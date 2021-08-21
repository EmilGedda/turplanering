{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Turplanering.API where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.IORef
import           Data.Tuple
import           Data.Word
import           Database.PostgreSQL.Simple
import           GHC.IO
import           Network.Wai
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           System.Random
import qualified Data.Text.Encoding         as T
import qualified Data.Vault.Lazy            as V

import           Turplanering.DB
import           Turplanering.Logger
import           Turplanering.Map
import qualified Turplanering.Config as Config

newtype Routes route = Routes
    {_get :: route :- "trails" :> Capture "area" Box :> Get '[JSON] Details}
    deriving (Generic)

data RequestContext = RequestContext
    { config :: Config.App
    , dbConn :: Connection
    , requestID :: RequestID
    }

data IDGen = RandomID | SequentialID

newtype RequestID = RequestID {getID :: Word16}
    deriving (ToJSON)

newtype ContextM a = ContextM
    -- drop Handler for IO and use MonadThrow and MonadCatch
    {runContext :: ReaderT RequestContext Handler a}
    deriving (Functor, Applicative, Monad, MonadReader RequestContext, MonadIO)

instance MonadStorage ContextM where
    getDetails box = withConnection (getDetails box) =<< asks dbConn

{-# NOINLINE requestIDKey #-}
requestIDKey :: V.Key RequestID
requestIDKey = unsafePerformIO V.newKey

{-# NOINLINE sequentialIDRef #-}
sequentialIDRef :: IORef Word16
sequentialIDRef = unsafePerformIO $ newIORef 1

{-# NOINLINE randomIDRef #-}
randomIDRef :: IORef StdGen
randomIDRef = unsafePerformIO $ newIORef =<< newStdGen

nextID :: IDGen -> IO Word16
nextID SequentialID = atomicModifyIORef' sequentialIDRef $ \id -> (id + 1, id)
nextID RandomID = atomicModifyIORef' randomIDRef $ swap . genWord16

routes :: MonadStorage m => Routes (AsServerT m)
routes =
    Routes
        { _get = getDetails
        }

toHandler :: RequestContext -> ContextM a -> Handler a
toHandler ctx handler = runReaderT (runContext handler) ctx

api :: RequestContext -> Application
api ctx = genericServeT (toHandler ctx) routes

getRequestID :: Request -> Maybe RequestID
getRequestID = V.lookup requestIDKey . vault

requestLogger :: Middleware
requestLogger app req resp = do
    logger Debug "serving http request"
        & field "method" (T.decodeUtf8 $ requestMethod req)
            . field "path" (T.decodeUtf8 $ rawPathInfo req)
            . field "from" (show $ remoteHost req)
            . fieldMaybe "reqID" (getRequestID req)
    app req resp
    where
        logger = consoleLogger Trace "http"

withRequestID :: IDGen -> (RequestID -> Application) -> Application
withRequestID rng app req resp = do
    number <- nextID rng
    let id' = RequestID number
        vault' = V.insert requestIDKey id' $ vault req
        req' = req{vault = vault'}
    app id' req' resp
