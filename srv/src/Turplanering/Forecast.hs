{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Turplanering.Forecast where

import           Control.Monad.Except    (MonadIO)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson.Types        (ToJSON)
import           Data.Foldable
import           Data.Maybe
import           Data.Text.Encoding
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Optics
import           Servant
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as B8
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Map.Strict         as M
import qualified Data.Text               as T
import qualified Xeno.DOM                as XML


type Layer = T.Text


newtype Layers = Layers [Layer]
    deriving stock (Generic)
    deriving anyclass (ToJSON)


instance FromHttpApiData Layers where
    parseUrlPiece = return . Layers . T.split (== ',')


data WMSTime = WMSTime
    { reference :: T.Text
    , validTimes :: T.Text -- WTS Periodicity Format
    }
    deriving (Generic, ToJSON)


newtype Forecast = Forecast {forecast :: M.Map Layer WMSTime}
    deriving stock (Generic)
    deriving anyclass (ToJSON)


makeFieldLabelsWith noPrefixFieldLabels ''Forecast


class MonadForecast m where
    getForecast :: Layers -> m Forecast
    default getForecast :: MonadIO m => Layers -> m Forecast
    getForecast (Layers layers) = liftIO $ do
        mgr <- getGlobalManager
        req <- parseRequest $ wtsURL layers
        res <- httpLbs req mgr

        return
            . over #forecast (M.filterWithKey (\k _ -> k `elem` layers))
            . parseXML
            . skipDoctype
            . LB.toStrict
            $ responseBody res


wtsURL :: [Layer] -> String
wtsURL layers =
    "https://wts.smhi.se/tile/"
        ++ T.unpack (T.intercalate "," layers)
        ++ "?SERVICE=WMS&REQUEST=GetCapabilities"


skipDoctype :: B.ByteString -> B.ByteString
skipDoctype = B.drop 2 . B8.dropWhile (/= ']')


parseXML :: B.ByteString -> Forecast
parseXML =
    Forecast
        . M.fromList
        . mapMaybe nodeToForecast
        . fold
        . either (const Nothing) getWMSLayers
        . XML.parse


getWMSLayers :: XML.Node -> Maybe [XML.Node]
getWMSLayers node = do
    cap <- getChild find "Capability" node
    layers <- getChild find "Layer" cap
    return $ XML.children layers


getText :: XML.Node -> Maybe T.Text
getText (XML.contents -> [XML.Text txt]) = Just $ decodeUtf8 txt
getText _ = Nothing


layerName :: XML.Node -> Maybe T.Text
layerName node = getText =<< getChild find "Name" node


nodeToForecast :: XML.Node -> Maybe (Layer, WMSTime)
nodeToForecast node = do
    name <- layerName node
    refTime <- extent "reftime" node
    validTime <- extent "time" node
    return (name, WMSTime refTime validTime)


extent :: B.ByteString -> XML.Node -> Maybe T.Text
extent ref parent = getText =<< find (\n -> ("name", ref) `elem` XML.attributes n) extents
    where
        extents = getChild filter "Extent" parent


getChild :: ((XML.Node -> Bool) -> [XML.Node] -> a) -> B.ByteString -> XML.Node -> a
getChild f tag = f (\n -> XML.name n == tag) . XML.children
