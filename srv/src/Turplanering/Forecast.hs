{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Turplanering.Forecast where

import           Control.Exception
import           Control.Monad.State.Strict
import           Data.Aeson.Types
import           Data.Either
import           Data.Foldable
import           Data.Maybe
import           Data.Text.Encoding
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Optics
import           Servant
import           Turplanering.Time
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Xeno.DOM                   as XML


type Layer = T.Text


newtype Layers = Layers {fromLayers :: [Layer]}
    deriving stock (Generic, Eq, Ord)
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
    deriving newtype (Semigroup, Monoid)
    deriving anyclass (ToJSON)


makeFieldLabelsWith noPrefixFieldLabels ''Forecast


newtype ForecastCache = ForecastCache {cache :: M.Map Layer (UTCTime, WMSTime)}


makeFieldLabelsWith noPrefixFieldLabels ''ForecastCache


data ForecastError = ParseError
    { xml :: B.ByteString
    , context :: String
    }


instance Show ForecastError where
    show (ParseError _ ctx) = "forecast parsing error: " <> ctx


instance Exception ForecastError


class Monad m => MonadForecast m where
    getForecast :: Layers -> m Forecast
    default getForecast ::
        (MonadTrans t, MonadForecast m1, m ~ t m1) =>
        Layers ->
        m Forecast
    getForecast = lift . getForecast


instance MonadForecast IO where
    getForecast :: Layers -> IO Forecast
    getForecast (Layers []) = return mempty
    getForecast (Layers layers) = do
        mgr <- getGlobalManager
        req <- parseRequest $ wtsURL layers
        res <- httpLbs req mgr

        return
            . over #forecast (M.filterWithKey (\k _ -> k `elem` layers))
            . parseXML
            . skipDoctype
            . LB.toStrict
            $ responseBody res


instance ( Monad (t m)
         , MonadState ForecastCache (t m)
         , MonadTime (t m)
         , MonadForecast m
         , MonadTrans t
         ) => MonadForecast (t m) where
    getForecast :: Layers -> t m Forecast
    getForecast (Layers layers) = do
        (miss, hit) <- partitionEithers <$> traverse inCache layers
        freshForecast <- lift (getForecast (Layers miss))
        insertIntoCache freshForecast
        return $ freshForecast <> Forecast (M.fromList hit)


inCache ::
    (MonadTime m, MonadState ForecastCache m) =>
    Layer ->
    m (Either Layer (Layer, WMSTime))
inCache layer = do
    now <- getCurrentTime
    cache <- use #cache
    return $ case M.lookup layer cache of
        Just (cachedTime, wmsTime)
            | diffUTCTime now cachedTime < 3600 -> Right (layer, wmsTime)
        _ -> Left layer

insertIntoCache :: (MonadTime m, MonadState ForecastCache m) => Forecast -> m ()
insertIntoCache forecast = do
    now <- getCurrentTime
    modifying' #cache
        . mappend
        . fmap (now,)
        $ view #forecast forecast


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


nodeToForecast :: XML.Node -> Maybe (Layer, WMSTime)
nodeToForecast node = do
    name <- layerName node
    refTime <- extent "reftime" node
    validTime <- extent "time" node
    return (name, WMSTime refTime validTime)


layerName :: XML.Node -> Maybe T.Text
layerName node = getText =<< getChild find "Name" node


extent :: B.ByteString -> XML.Node -> Maybe T.Text
extent ref parent = do
    node <- find (\n -> ("name", ref) `elem` XML.attributes n) extents
    getText node
    where
        extents = getChild filter "Extent" parent


getWMSLayers :: XML.Node -> Maybe [XML.Node]
getWMSLayers node = do
    cap <- getChild find "Capability" node
    layers <- getChild find "Layer" cap
    return $ XML.children layers


getChild :: ((XML.Node -> Bool) -> [XML.Node] -> a) -> B.ByteString -> XML.Node -> a
getChild f tag = f (\n -> XML.name n == tag) . XML.children


getText :: XML.Node -> Maybe T.Text
getText (XML.contents -> [XML.Text txt]) = Just $ decodeUtf8 txt
getText _ = Nothing
