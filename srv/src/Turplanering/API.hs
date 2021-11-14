{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Turplanering.API where

import           Control.Monad.Except
import           Control.Monad.State        (MonadState (..))
import           Control.Monad.Catch        hiding (Handler)
import           Control.Monad.Reader
import           Colog.Core                 hiding (Debug, Info)
import           Data.Aeson
import           Data.IORef
import           Data.Tuple
import           Data.Word
import           Database.PostgreSQL.Simple
import           Network.Wai
import           Network.HTTP.Types
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           System.Random
import           GHC.IO (unsafePerformIO)
import           Optics                     hiding ((&))
import qualified Control.Monad.Catch        as Exception (Handler (Handler))
import qualified Data.Vault.Lazy            as V

import           Turplanering.DB
import           Turplanering.Forecast
import           Turplanering.Map
import           Turplanering.Time
import           Turplanering.Log
import qualified Turplanering.Config   as Config
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
    , request :: Request
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

type Hook = forall a. App a -> App a

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


withRequest :: (Request -> Application) -> Application
withRequest app req res = do
    let res' response = do
            res response
    app req req res'

api :: Hook -> RequestContext -> Application
api hook ctx req res = do
    ref <- newIORef (void . return)
    let res' r = do
            f <- readIORef ref
            f r
            res r
    genericServeT (toHandler ctx . hook . requestLogger ref) routes req res'


logInjectRequestID :: (MonadIO m, MonadReader RequestContext m) => IDGen -> m a -> m a
logInjectRequestID rng app = do
    id <- nextID rng
    local (modifyLogWith $ field "reqID" id) app

requestLogger :: (MonadIO m, MonadTime m, WithLog RequestContext m) => IORef (Response -> IO ()) -> m a -> m a
requestLogger ref app = do
    req <- asks request
    logger <- asks logAction

    log' Debug "serving http request"
        Fields.do
            field "method" $ requestMethod req
            field "path"   $ rawPathInfo req
            field "from"   . show $ remoteHost req

    start <- getCurrentTime
    ret <- app
    end <- getCurrentTime

    let action res =
            logger <&
                (newMsg "http" Trace "served http request"
                    & Fields.do
                        field "elapsed" $ diffTimestamp end start
                        field "status"  . statusCode $ responseStatus res)

    liftIO . writeIORef ref $ action

    return ret

