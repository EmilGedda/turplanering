module Main where

import           Control.Concurrent
import           Control.Exception
import           Data.Aeson
import           Data.Function
import           Data.Geospatial
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.TimeIt
import           Text.Printf
import qualified Data.Sequence           as Seq
import qualified Streamly.Prelude        as Streamly

naturkartan :: [(Int, String)]
naturkartan = zip [1 ..] $ map baseUrl [1 .. 31]
    where
        baseUrl n =
            "http://api.naturkartan.se/v2/sites/search.geojson"
                ++ "?search%5Bguide_ids%5D%5B%5D="
                ++ show n
                ++ "&search%5Bimportance%5D=3"

type GeoJSON = GeoFeatureCollection Value

data ScrapingException = DecodeError Int String
    deriving (Show)

instance Exception ScrapingException

fetch :: Manager -> Int -> String -> IO GeoJSON
fetch mgr n url = runInUnboundThread $ do
    printf "Fetching geojson #%d\n" n
    (elapsed, res) <- timeItT $ httpLbs (parseRequest_ url) mgr
    printf "Fetched geojson #%02d in %0.3fs\n" n elapsed
    case eitherDecode' $ responseBody res of
        Left msg -> throwIO $ DecodeError n msg
        Right geojson -> return geojson

mergeGeoJSON :: GeoJSON -> GeoJSON -> GeoJSON
mergeGeoJSON a b =
    GeoFeatureCollection Nothing $
        _geofeatures a Seq.>< _geofeatures b

serialize :: ToJSON a => a -> IO ()
serialize json = encodeFile file json *> printf "Saved data in %s\n" file
    where
        file = "naturkartan.geojson"

main :: IO ()
main = do
    mgr <- newTlsManagerWith $ tlsManagerSettings{managerConnCount = 50}
    Streamly.fromList naturkartan
        & Streamly.fromSerial
        & Streamly.mapM (uncurry $ fetch mgr)
        & Streamly.fromParallel
        & Streamly.foldr mergeGeoJSON (GeoFeatureCollection Nothing Seq.empty)
        >>= serialize
