{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.IORef
import           Data.String
import           Database.PostgreSQL.Simple
import           Dhall                       (auto, input)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip
import           System.Directory
import           System.FilePath
import qualified Data.ByteString             as B
import qualified Data.Map                    as M
import qualified Data.Text                   as T

import Turplanering.API
import Turplanering.Config   hiding (Info, LogLevel, Trace, logger)
import Turplanering.DB
import Turplanering.Forecast
import Turplanering.Logger


logger :: LogType t => LogLevel -> B.ByteString -> t
logger = consoleLogger Trace "main"


main :: IO ()
main = do
    cfgDir <- getXdgDirectory XdgConfig "turplanering"
    let cfgPath = cfgDir </> "config.dhall"
    logger Info "loading config"
        & field "path" cfgPath

    config <- input auto . T.pack $ cfgPath
    let dbConfig = db config
    logger Info "connecting to DB"
        & field "url" (dbAddr dbConfig)
            . field "port" (dbPort dbConfig)

    dbConn <- connect $ getConnectionInfo dbConfig
    forecastCache <- newIORef $ ForecastCache M.empty

    let httpConfig = http config
        context = RequestContext config dbConn forecastCache
        warpSettings =
            setPort (fromIntegral $ httpPort httpConfig)
                . setHost (fromString $ httpAddr httpConfig)
                $ defaultSettings

    logger Info "serving requests"
        & field "url" (httpAddr httpConfig)
            . field "port" (httpPort httpConfig)

    runSettings warpSettings
        . withRequestID RandomID
        $ gzip def
            . requestLogger
            . simpleCors
            . api
            . context
