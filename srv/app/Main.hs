{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.Morpheus
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Turplanering.API
import           Turplanering.Logger
import qualified Data.ByteString as B
import qualified Turplanering.DB as DB


logger :: LogType t => LogLevel -> B.ByteString -> t
logger = consoleLogger Trace "main"

gqlApi :: DB.Handle IO Application
gqlApi = do
    conn <- ask
    return $ \req resp ->
        strictRequestBody req
            >>= DB.withConnection conn . interpreter gqlResolver
            >>= resp . responseLBS status200 [] -- TODO: use bracket for error handling

main :: IO ()
main = do
    logger Info "Generating schema...\n"
    threadDelay 1000000
   -- BL.putStrLn $ toGraphQLDocument (Proxy :: Proxy (GQLAPI IO))
    logger Info "Testing logger output"
    threadDelay 1000000
    logger Trace "trace log message"
        & field "field one" ("value one" :: String)
        . field "two"    (2 :: Int)
    threadDelay 1000000
    logger Debug "debug message in logger"
        & field "address" ("localhost" :: String)
    threadDelay 1000000
    logger Error  "very dangerous error here"
        & field "statusCode" (500 :: Int)
    threadDelay 1000000
    logger Warning  "a little warning coming through"
        & field "text" ("string" :: String)
        . field "elapsed" ("time" :: String)
        . field "lorem"   ("ipsum" :: String)
    threadDelay 1000000
    logger Info "serving GraphQL"
        & field "foo" ("bar" :: String)
        . field "int"    (1234 :: Int)
    threadDelay 1000000
    api <- DB.withConfig DB.devConfig gqlApi
    run 4000 api
