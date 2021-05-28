{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Morpheus
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Turplanering.API
import Turplanering.DB
import Turplanering.Logger
import Data.Data
import Data.Morpheus.Document
import qualified Data.ByteString.Lazy.Char8 as B

gqlApi :: Config -> Application
gqlApi cfg req resp = strictRequestBody req
                  >>= runHandle cfg . interpreter gqlResolver
                  >>= resp . responseLBS status200 []

main :: IO ()
main = do
    let log = consoleLogger "main"
    log Info "Generating schema:\n"
    B.putStrLn $ toGraphQLDocument (Proxy :: Proxy (GQLAPI IO))
    log Info "Serving GraphQL on localhost:4000"
    run 4000 (gqlApi devConfig)
