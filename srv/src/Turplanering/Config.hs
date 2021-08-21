{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Turplanering.Config where

import Data.Word
import Dhall

data LogLevel
    = Trace
    | Debug
    | Info
    | Warning
    | Error
    | Fatal
    | Silent
    deriving stock (Generic, Show, Enum, Bounded)
    deriving anyclass (FromDhall, ToDhall)

data LogStyle
    = Auto
    | Pretty
    | Structured
    deriving stock (Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

data Environment
    = Production
    | Development
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall)

data HTTP = HTTP
    { httpPort :: Word16
    , httpAddr :: String
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

data Logger = Logger
    { level :: LogLevel
    , style :: LogStyle
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

data DB = DB
    { dbAddr :: String
    , dbPort :: Word16
    , username :: String
    , password :: String
    , database :: String
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

data App = App
    { db :: DB
    , http :: HTTP
    , logger :: Logger
    , environ :: Environment
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromDhall, ToDhall)
