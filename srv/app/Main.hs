{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Data.String
import           Database.PostgreSQL.Simple
import           Dhall (input, auto)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           System.Directory
import           System.FilePath
import           Turplanering.API
import           Turplanering.Config hiding (LogLevel, logger, Trace, Info)
import           Turplanering.DB
import           Turplanering.Logger
import qualified Data.ByteString     as B
import qualified Data.Text           as T


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
        & field "url"  (dbAddr dbConfig)
        . field "port" (dbPort dbConfig)

    dbConn <- connect $ getConnectionInfo dbConfig

    let httpConfig   = http config
        appContext   = AppContext config dbConn
        warpSettings = setPort (fromIntegral $ httpPort httpConfig)
                     . setHost (fromString   $ httpAddr httpConfig)
                     $ defaultSettings

    logger Info "serving requests"
        & field "url"  (httpAddr httpConfig)
        . field "port" (httpPort httpConfig)

    runSettings warpSettings
        . withRequestID
        $ gzip def
        . requestLogger
        . api appContext
