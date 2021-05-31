{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Turplanering.Logger where

import           Prelude               hiding (log)
import           Control.Monad
import           Control.Exception
import           Data.Aeson            hiding (Error)
import           Data.Bool
import           Data.Function
import           Data.Text.Encoding
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.TH
import           System.IO
import           System.Exit
import           System.Console.ANSI.Types
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Char8 as B8
import Data.Char
import System.Console.ANSI

data LogLevel = Trace | Debug | Info | Warning | Error | Fatal
    deriving (Eq, Ord, Show, Generic, ToJSON)

data Field = Field !B.ByteString !Value deriving Show

-- TODO: Add Maybe Timestamp
data LogEntry =
    LogEntry {
        _logNamespace :: !B.ByteString,
        _logLevel :: !LogLevel,
        _message :: !B.ByteString,
        _fields :: ![Field]
    } deriving Show


-- TODO: Optimize data and fields ToJSON
instance ToJSON LogEntry where
    toJSON (LogEntry ns lvl str fields) =
        let keyval (Field k v) = decodeUtf8 k .= v
        in object [ "namespace" .= decodeUtf8 ns
                  , "level"     .= lvl
                  , "message"   .= decodeUtf8 str
                  , "data"      .= object (map keyval fields)]

    toEncoding (LogEntry ns lvl str fields) =
        let keyval (Field k v) = decodeUtf8 k .= v
        in pairs ("namespace" .= decodeUtf8 ns
               <> "level"     .= lvl
               <> "message"   .= decodeUtf8 str
               <> "data"      .= object (map keyval fields))


type LogConsumer = LogAction -> IO ()

data LogAction = LogAction {
        _formatter :: !LogConsumer,
        _entry :: !LogEntry
    }

makeLenses ''LogEntry
makeLenses ''LogAction


-- TODO: Replace (... -> ...) -> t with LogAction -> t
class LogType t where
    log' :: LogConsumer -> B.ByteString -> LogLevel -> B.ByteString -> [Field] -> t

instance LogType LogAction where
    log' f ns lvl msg = LogAction f . LogEntry ns lvl msg

instance (a ~ ()) => LogType (IO a) where
    log' f ns lvl msg fields = f (LogAction f $ LogEntry ns lvl msg fields)

instance (a ~ (LogAction -> LogAction), LogType r) => LogType (a -> r) where
    log' f ns lvl msg args = \mod -> log' (f . mod) ns lvl msg args

type Modifier = forall r. LogType r => LogAction -> r

liftAction :: LogType r => LogAction -> r
liftAction (LogAction f (LogEntry ns lvl str args))
  = log' f ns lvl str args

overMsg :: Lens' LogEntry a -> (a -> a) -> Modifier
overMsg f g = liftAction . over (entry . f) g

setMsg :: Lens' LogEntry a -> a -> Modifier
setMsg f x = overMsg f (const x)


field :: ToJSON a => B.ByteString -> a -> Modifier
field key value = overMsg fields (\args -> Field key (toJSON value):args)

namespace :: B.ByteString -> Modifier
namespace ns = overMsg logNamespace (\n -> n <> "." <> ns)

level :: LogLevel -> Modifier
level lvl = overMsg logLevel (max lvl)

debug, info, warn, fatal:: Modifier
debug = level Debug
info  = level Info
warn  = level Warning
fatal = level Fatal

logStr :: B.ByteString -> Modifier
logStr = setMsg message

err :: Exception e => e -> Modifier
err e = field "error" (displayException e) . level Error

infixl 1 &
(&) :: a -> (a -> b) -> b
(&) = (Data.Function.&)

newLogger :: LogType r => LogConsumer -> B.ByteString -> LogLevel -> B.ByteString -> r
newLogger f ns lvl msg = log' f ns lvl msg []

structuredLogger, consoleLogger :: LogType r => B.ByteString -> LogLevel -> B.ByteString -> r
structuredLogger = newLogger $ (console <*> jsonFormat) . _entry
consoleLogger    = newLogger $ (console <*> readableFormat) . _entry

shortFmt :: LogLevel -> B.ByteString
shortFmt Trace   = "TRAC"
shortFmt Debug   = "DEBU"
shortFmt Info    = "INFO"
shortFmt Warning = "WARN"
shortFmt Error   = "ERRO"
shortFmt Fatal   = "FATA"

levelColor :: LogLevel -> SGR
levelColor lvl = case lvl of
        Trace   -> SetColor Foreground Dull White
        Debug   -> SetColor Foreground Vivid White
        Info    -> SetColor Foreground Vivid Blue
        Warning -> SetColor Foreground Dull Yellow
        Error   -> SetColor Foreground Dull Red
        Fatal   -> SetColor Foreground Vivid Red

levelStyle :: LogLevel -> [SGR]
levelStyle lvl = case lvl of
        Error -> [SetBlinkSpeed SlowBlink]
        Fatal -> SetConsoleIntensity BoldIntensity:levelStyle Error
        _     -> []

console :: LogEntry -> B.ByteString -> IO ()
console (LogEntry _ lvl _ _) msg = do
    C.hPutStrLn handle msg
    when (lvl >= Fatal) exitFailure
    where handle = bool stdout stderr $ lvl >= Warning

readableFormat :: LogEntry -> B.ByteString
readableFormat (LogEntry _ lvl msg fields) =
    let fmt (Field k v) = color k <> "=" <> L.toStrict (encode v)
        rightPad str minLen = mappend str .  B8.replicate (minLen - B.length str)
        capitalize s = B8.pack [toUpper $ B8.head s] <> B.drop 1 s
        color f = B8.pack (setSGRCode [levelColor lvl]) <> f
               <> B8.pack (setSGRCode [])
        dispLevel = (<>) <$> B8.pack . setSGRCode . levelStyle
                         <*> color . shortFmt
    in C.intercalate " "
        $ dispLevel lvl
        : rightPad (capitalize msg) 40 ' '
        : map fmt fields

jsonFormat :: LogEntry -> B.ByteString
jsonFormat = L.toStrict . encode

autoFormat :: IO (LogEntry -> B.ByteString)
autoFormat = hIsTerminalDevice stdout
         <&> \case
            True -> readableFormat
            _    -> jsonFormat
