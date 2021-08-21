module Main where

import Control.Exception
import Data.Aeson
import Data.Function
import Data.Geospatial
import Network.Curl
import qualified Data.Sequence as Seq
import qualified Streamly.Prelude as Streamly


naturkartan :: [(Int, String)]
naturkartan = zip [1..] $ map baseUrl [1..31]
    where baseUrl n = "https://api.naturkartan.se/v2/sites/search.geojson"
                    ++ "?search%5Bguide_ids%5D%5B%5D="
                    ++ show n ++ "&search%5Bimportance%5D=3"


type GeoJSON = GeoFeatureCollection Value


data ScrapingException = DecodeError Int String
    deriving Show

instance Exception ScrapingException

fetch :: Int -> String -> IO GeoJSON
fetch n url = do
    body <- snd <$> curlGetString_ url []
    putStr "Fetched geojson #"
    print n
    case eitherDecode body of
      Left msg -> throwIO $ DecodeError n msg
      Right geojson -> return geojson

mergeGeoJSON :: GeoJSON -> GeoJSON -> GeoJSON
mergeGeoJSON a b = GeoFeatureCollection Nothing
    $ _geofeatures a Seq.>< _geofeatures b


serialize :: ToJSON a => a -> IO ()
serialize json = encodeFile file json
         *> putStr "Saved data in "
         *> putStrLn file
    where file = "naturkartan.geojson"

main :: IO ()
main = withCurlDo
        $ Streamly.fromList naturkartan
        & Streamly.fromSerial
        & Streamly.mapM (uncurry fetch)
        & Streamly.fromAsync
        & Streamly.foldr mergeGeoJSON  (GeoFeatureCollection Nothing Seq.empty)
        >>= serialize
