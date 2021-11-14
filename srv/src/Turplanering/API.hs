{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Turplanering.API where

import           Colog.Core                 hiding (Debug, Info)
import           Control.Monad.Catch        hiding (Handler)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State        (MonadState (..))
import           Data.Aeson
import           Data.IORef
import           Data.Tuple
import           Data.Word
import           Database.PostgreSQL.Simple
import           GHC.IO                     (unsafePerformIO)
import           Network.HTTP.Types
import           Network.Wai                hiding (Middleware)
import           Optics                     hiding ((&))
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           System.Random
import qualified Control.Monad.Catch        as Exception (Handler (Handler))
import qualified Data.Vault.Lazy            as V

import           Turplanering.DB
import           Turplanering.Forecast
import           Turplanering.Log
import           Turplanering.Map
import           Turplanering.Time
import qualified Turplanering.Config     as Config
import qualified Turplanering.Log.Fields as Fields

data Routes route = Routes
    { _getDetails :: route :- "trails" :> Capture "area" Box :> Get '[JSON] Details
    , _getForecast :: route :- "forecast" :> Capture "layers" Layers :> Get '[JSON] Forecast
    }
    deriving (Generic)


data IDGen = RandomID | SequentialID


newtype RequestID = RequestID {getID :: Word16}
    deriving ToJSON


data RequestContext = RequestContext
    { config :: Config.App
    , dbConn :: Connection
    , forecastCache :: IORef ForecastCache
    , logAction :: LogAction IO FieldMessage
    }


newtype ContextM m a = ContextM
    {runContext :: ReaderT RequestContext m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadTrans
        , MonadReader RequestContext
        , MonadIO
        , MonadThrow
        , MonadCatch
        )


makeFieldLabelsNoPrefix ''RequestContext
logInNamespace "http"


type App = ContextM IO

instance HasLog RequestContext FieldMessage IO where
    getLogAction = view #logAction
    setLogAction = set #logAction

instance HasLog RequestContext FieldMessage (ContextM IO) where
    getLogAction = hoistLogAction liftIO . view #logAction
    setLogAction action = set #logAction =<< flip unliftAction action


unliftAction :: RequestContext -> LogAction (ContextM m) a -> LogAction m a
unliftAction ctx = hoistLogAction (flip runReaderT ctx . runContext)


instance MonadStorage App where
    getDetails box = withConnection (getDetails box) =<< asks dbConn


instance MonadState ForecastCache App where
    state f = do
        ref <- asks forecastCache
        lift $ atomicModifyIORef' ref (swap . f)


instance MonadTime App

type Middleware = (RequestContext -> Application) -> RequestContext -> Application

{-# NOINLINE responseLoggerKey #-}
responseLoggerKey :: V.Key Response
responseLoggerKey = unsafePerformIO V.newKey


{-# NOINLINE sequentialIDRef #-}
sequentialIDRef :: IORef Word16
sequentialIDRef = unsafePerformIO $ newIORef 1


{-# NOINLINE randomIDRef #-}
randomIDRef :: IORef StdGen
randomIDRef = unsafePerformIO $ newIORef =<< newStdGen


nextID :: MonadIO m => IDGen -> m Word16
nextID SequentialID = liftIO $ atomicModifyIORef' sequentialIDRef $ \id -> (id + 1, id)
nextID RandomID = liftIO $ atomicModifyIORef' randomIDRef $ swap . genWord16


routes :: (MonadForecast m, MonadStorage m) => Routes (AsServerT m)
routes =
    Routes
        { _getDetails = getDetails
        , _getForecast = getForecast
        }


modifyLogWith :: (FieldMessage -> FieldMessage) -> RequestContext -> RequestContext
modifyLogWith mod = over #logAction (cmap mod)


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

withRequestID :: IDGen -> Middleware
withRequestID rng app ctx req res = do
    reqID <- nextID rng
    let ctx' = modifyLogWith (field "reqID" reqID) ctx
    app ctx' req res

logRequest :: Middleware
logRequest app ctx req res = do
    let log lvl msg fields = getLogAction ctx <& fields (newMsg "http" lvl msg)

    log Debug "serving http request"
        Fields.do
            field "method" $ requestMethod req
            field "path"   $ rawPathInfo req
            field "from"   . show $ remoteHost req

    start <- getCurrentTime
    app ctx req
        $ \response -> do
            end <- getCurrentTime
            log Trace "served http request"
                Fields.do
                    field "elapsed" $ diffTimestamp end start
                    field "status"  . statusCode $ responseStatus response
            res response



api :: RequestContext -> Application
api ctx = genericServeT (toHandler ctx) routes

