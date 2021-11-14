{-# OPTIONS_GHC -fno-warn-orphans #-}

module Turplanering.Log.Types where

import Data.Aeson
import qualified Data.ByteString         as B
import qualified Data.Text.Encoding      as T

instance ToJSON B.ByteString where
    toJSON = toJSON . T.decodeUtf8
    toEncoding = toEncoding . T.decodeUtf8
