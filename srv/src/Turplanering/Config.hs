{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Turplanering.Config where

import Dhall

data LogLevel = Trace
               | Debug
               | Info
               | Warning
               | Error
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

data LogStyle = Auto
               | Pretty
               | Structured
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

data Environment = Production
                 | Development
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

newtype GQL = GQL
    { port :: Natural }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

data Logger = Logger
    { level :: LogLevel
    , style :: LogStyle
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

data DB = DB
    { address  :: String
    , port     :: Natural
    , username :: String
    , password :: String
    , database :: String
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

data App = App
    { db      :: DB
    , gql     :: GQL
    , logger  :: Logger
    , environ :: Environment
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall, ToDhall)
