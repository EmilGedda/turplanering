{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Turplanering.Logger
       ( module Turplanering.Logger
       , (Data.Function.&)
       ) where

import           Prelude                    hiding (log)
import           Control.Monad
import           Control.Exception
import           Data.Aeson                 hiding (Error)
import           Data.Bool
import           Data.Function
import           Data.Text.Encoding
import           GHC.Generics
import           Control.Lens               hiding ((.=))
import           System.IO
import           System.Exit
import           System.Console.ANSI.Types
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Char8      as B8
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
type Formatter = Bool -> LogEntry -> B8.ByteString
type Logger = forall r.  LogType r => LogLevel -> B.ByteString -> LogLevel -> B.ByteString -> r


liftAction :: LogType r => LogAction -> r
liftAction (LogAction f (LogEntry ns lvl str args))
  = log' f ns lvl str args

overMsg :: Lens' LogEntry a -> (a -> a) -> Modifier
overMsg f g = liftAction . over (entry . f) g

setMsg :: Lens' LogEntry a -> a -> Modifier
setMsg f x = overMsg f (const x)


field :: ToJSON a => B.ByteString -> a -> Modifier
field key value = overMsg fields (\args -> Field key (toJSON value):args)

fieldMaybe :: ToJSON a => B.ByteString -> Maybe a -> Modifier
fieldMaybe key (Just value) = overMsg fields (\args -> Field key (toJSON value):args)
fieldMaybe _ Nothing        = liftAction

namespace :: B.ByteString -> Modifier
namespace ns = overMsg logNamespace (\n -> n <> "." <> ns)

level :: LogLevel -> Modifier
level lvl = overMsg logLevel (max lvl)

debug, info, warn, fatal :: Modifier
debug = level Debug
info  = level Info
warn  = level Warning
fatal = level Fatal

logStr :: B.ByteString -> Modifier
logStr = setMsg message

err :: Exception e => e -> Modifier
err e = field "error" (displayException e) . level Error

newLogger :: LogType r => LogLevel -> LogConsumer -> B.ByteString -> LogLevel -> B.ByteString -> r
newLogger verbosity consumer namespace level message = log' output namespace level message []
  where output = filterLogs ((>= verbosity) . view logLevel) consumer

mkLogger :: (a -> LogEntry -> IO ()) -> a -> Logger
mkLogger out format v = newLogger v $ out format . _entry

structuredLogger :: Logger
structuredLogger = mkLogger console jsonFormat

consoleLogger :: Logger
consoleLogger = mkLogger console readableFormat


shortFmt :: LogLevel -> B.ByteString
shortFmt lvl = case lvl of
         Trace   -> "TRAC"
         Debug   -> "DEBU"
         Info    -> "INFO"
         Warning -> "WARN"
         Error   -> "ERRO"
         Fatal   -> "FATA"

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

console :: Formatter -> LogEntry -> IO ()
console formatter entry@(LogEntry _ lvl _ _) = do
    color <- hSupportsANSIColor handle
    let msg = formatter color entry
    C.hPutStrLn handle msg
    when (lvl >= Fatal) exitFailure
    where handle = bool stdout stderr $ lvl >= Warning

filterLogs :: Applicative f =>
    (LogEntry -> Bool) -> (LogAction -> f ()) -> LogAction -> f ()
filterLogs p f x | p (_entry x) = f x
                 | otherwise = pure ()

readableFormat :: Formatter
readableFormat hasColor (LogEntry _ lvl msg fields) =
    let pack = B8.pack . setSGRCode
        fmt (Field k v) = color k <> "=" <> L.toStrict (encode v)
        rightPad str minLen = mappend str .  B8.replicate (minLen - B.length str)
        capitalize s = B8.pack [toUpper $ B8.head s] <> B.drop 1 s
        dispLevel = (<>) <$> pack . levelStyle <*> color . shortFmt
        color f | hasColor = pack [levelColor lvl] <> f <> pack []
                | otherwise = f
    in C.intercalate " "
       $ dispLevel lvl
       : rightPad (capitalize msg) 30 ' '
       : map fmt fields

jsonFormat :: Formatter
jsonFormat _ = L.toStrict . encode

autoFormat :: IO Formatter
autoFormat = hIsTerminalDevice stdout
         <&> \case
            True -> readableFormat
            _    -> jsonFormat
