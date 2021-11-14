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
import           Text.Colour.Capabilities.FromEnv
import qualified Data.Map                    as M
import qualified Data.Text                   as T

import           Turplanering.API
import           Turplanering.Config   hiding (Info, LogLevel, Trace, logger)
import           Turplanering.DB
import           Turplanering.Forecast
import           Turplanering.Log
import qualified Turplanering.Log.Fields as Fields


main :: IO ()
main = do

    termCaps <- getTerminalCapabilitiesFromEnv
    cfgDir <- getXdgDirectory XdgConfig "turplanering"
    let cfgPath = cfgDir </> "config.dhall"

    config <- input auto . T.pack $ cfgPath

    let
        orInProd :: a -> a -> a
        orInProd a b
            | environ config == Development = a
            | otherwise = b

        fmt = logPrettyStdout termCaps `orInProd` logStructuredStdout

        action = cmapM timestampMessage fmt
        logger ns lvl msg fields = action <& (newMsg ns lvl msg & fields)
        log =  logger "main"

    log Info "loaded config"
        Fields.do
            field "path" cfgPath

    let dbConfig = db config

    log Info "connecting to DB"
        Fields.do
            field "url"  $ dbAddr dbConfig
            field "port" $ dbPort dbConfig

    dbConn <- connect $ getConnectionInfo dbConfig
    forecastCache <- newIORef $ ForecastCache M.empty

    let httpConfig = http config
        context = RequestContext config dbConn forecastCache action
        warpSettings =
            setPort (fromIntegral $ httpPort httpConfig)
                . setHost (fromString $ httpAddr httpConfig)
                $ defaultSettings

    log Info "serving requests"
        Fields.do
            field "url"  $ httpAddr httpConfig
            field "port" $ httpPort httpConfig

    runSettings warpSettings
        . withRequest
        $ gzip def
            . simpleCors
            . api
                (logInjectRequestID (SequentialID `orInProd` RandomID))
            . context
