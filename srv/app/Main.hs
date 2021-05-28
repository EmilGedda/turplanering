{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Reader
import Data.Morpheus
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Turplanering.API
import Turplanering.DB
import Turplanering.Logger
import Data.Data
import Data.Morpheus.Document
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL


logger :: B.ByteString -> LogLevel -> B.ByteString -> IO ()
logger = consoleLogger

gqlApi :: Handle IO Application
gqlApi = do
    conn <- ask
    return $ \req resp ->
        strictRequestBody req
            >>= withConnection conn . interpreter gqlResolver
            >>= resp . responseLBS status200 []

main :: IO ()
main = do
    logger "main" Info "Generating schema:\n"
    BL.putStrLn $ toGraphQLDocument (Proxy :: Proxy (GQLAPI IO))
    logger "main" Info "serving GraphQL on localhost:4000"
    run 4000 =<< connectDB devConfig gqlApi
