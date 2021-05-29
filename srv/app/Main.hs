{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad.Reader
import           Data.Morpheus
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types
import           Turplanering.API
import           Turplanering.DB
import           Turplanering.Logger
import           Data.Data
import           Data.Morpheus.Document
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL


logger :: LogType t => LogLevel -> B.ByteString -> t
logger = consoleLogger "main"

gqlApi :: Handle IO Application
gqlApi = do
    conn <- ask
    return $ \req resp ->
        strictRequestBody req
            >>= withConnection conn . interpreter gqlResolver
            >>= resp . responseLBS status200 [] -- TODO: use bracket for error handling

main :: IO ()
main = do
    logger Info "Generating schema:\n"
    BL.putStrLn $ toGraphQLDocument (Proxy :: Proxy (GQLAPI IO))
    logger Info "serving GraphQL"
        & field "address" ("localhost" :: String)
        . field "port"    (4000 :: Int)
    api <- connectDB devConfig gqlApi
    run 4000 api
