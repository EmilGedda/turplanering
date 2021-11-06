{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Turplanering.API where

import           Control.Monad.Catch        hiding (Handler)
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
import qualified Control.Monad.Catch        as Exception (Handler (Handler))
import qualified Data.Text.Encoding         as T
import qualified Data.Vault.Lazy            as V

import           Control.Monad.Except
import           Control.Monad.State   (MonadState (..))
import           Turplanering.DB
import           Turplanering.Forecast
import           Turplanering.Logger
import           Turplanering.Map
import           Turplanering.Time
import qualified Turplanering.Config   as Config


data Routes route = Routes
    { _getDetails :: route :- "trails" :> Capture "area" Box :> Get '[JSON] Details
    , _getForecast :: route :- "forecast" :> Capture "layers" Layers :> Get '[JSON] Forecast
    }
    deriving (Generic)


data RequestContext = RequestContext
    { config :: Config.App
    , dbConn :: Connection
    , forecastCache :: IORef ForecastCache
    , requestID :: RequestID
    }


data IDGen = RandomID | SequentialID


newtype RequestID = RequestID {getID :: Word16}
    deriving (ToJSON)


newtype ContextM m a = ContextM
    {runContext :: ReaderT RequestContext m a}
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadTrans
        , MonadReader RequestContext
        , MonadIO
        , MonadThrow
        , MonadCatch
        )


type App = ContextM IO


instance MonadStorage App where
    getDetails box = withConnection (getDetails box) =<< asks dbConn


instance MonadState ForecastCache App where
    state f = do
        ref <- asks forecastCache
        lift $ atomicModifyIORef' ref (swap . f)


instance MonadTime App


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


routes :: (MonadForecast m, MonadStorage m) => Routes (AsServerT m)
routes =
    Routes
        { _getDetails = getDetails
        , _getForecast = getForecast
        }


toHandler :: RequestContext -> App a -> Handler a
toHandler ctx handler = handleErrors $ runReaderT (runContext handler) ctx


handleErrors :: IO a -> Handler a
handleErrors action =
    Handler . ExceptT $
        fmap Right action
            `catches` fmap (fmap Left) exceptionHandlers


exceptionHandlers :: Monad m => [Exception.Handler m ServerError]
exceptionHandlers =
    [ Exception.Handler (\(e :: ServerError) -> return e)
    , Exception.Handler (\(_ :: SomeException) -> return err500)
    ]


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
