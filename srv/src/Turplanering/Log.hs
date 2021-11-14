{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Turplanering.Log
    ( module Colog.Core
    , FieldMessage(..)
    , TimedMessage(..)
    , Field(..)
    , ToPrettyLog(..)
    , LogEncodable
    , ViaShow
    , ViaQuoted
    , ViaJSON
    , LogLevel(..)
    , HasFields(..)
    , HasNamespace(..)
    , (Turplanering.Log.&)
    , WithLog
    , LogModifier
    , FieldLogger
    , MessageLogger
    , logStructuredStdout
    , logPrettyStdout
    , logInNamespace
    , timestampMessage
    , newMsg
    , tag
    , field
    , fieldMaybe
    ) where

import           Colog.Core              hiding (Error, Debug, Warning, Info)
import           Control.Monad.IO.Class  (liftIO, MonadIO)
import           Data.Aeson              hiding (Error)
import           Control.Monad.Reader    (MonadReader, asks)
import           Data.Aeson.Encoding
import           Data.Aeson.Text
import           Data.Aeson.Types        (Pair)
import           Data.Foldable
import           Data.Sequence           (Seq(..), (><))
import           GHC.Generics
import           Optics                  hiding ((&))
import           System.IO               (stdout)
import           Text.Colour
import           Turplanering.Time
import           Language.Haskell.TH
import           Turplanering.Log.Types ()
import qualified Data.Function
import qualified Data.ByteString.Builder as Bldr
import qualified Data.Sequence           as Seq
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL

class ToPrettyLog a where
    prettyChunks :: a -> Seq Chunk

type LogEncodable a = (ToJSON a, ToPrettyLog a)

newtype ViaShow a = ViaShow a
newtype ViaQuoted a = ViaQuoted a
newtype ViaJSON a = ViaJSON a

instance ToJSON a => ToPrettyLog (ViaJSON a) where
    prettyChunks (ViaJSON a) = return . chunk . TL.toStrict $ encodeToLazyText a

instance Show a => ToPrettyLog (ViaShow a) where
    prettyChunks (ViaShow a) = return $ chunk (T.pack $ show a)

instance ToPrettyLog a => ToPrettyLog (ViaQuoted a) where
    prettyChunks (ViaQuoted a) = (chunk "\"" :<| prettyChunks a) :|> chunk "\""


deriving via (ViaJSON a) instance {-# OVERLAPPABLE #-} ToJSON a => ToPrettyLog a
deriving via (ViaShow (a, b)) instance (Show a, Show b) => ToPrettyLog (a, b)
deriving via (ViaQuoted (ViaShow (Maybe a))) instance Show a => ToPrettyLog (Maybe a)

instance ToPrettyLog NominalDiffTime where
    prettyChunks duration
        | duration > 5     = fore brightRed <$> format duration "s"
        | duration > 1     = fore brightYellow <$> format duration "s"
        | duration > 0.001 = format (duration * 1e3) "ms"
        | otherwise        = format (duration * 1e6) "µs"
        where
            limitDecimals n txt = let (first, rest) = T.break (=='.') txt
                                   in first <> T.take n rest
            format d suffix = return
                        . chunk
                        . flip (<>) suffix
                        . limitDecimals 3
                        $ formatTime "%12Es" d

data LogLevel = Trace
              | Debug
              | Info
              | Warning
              | Error
              | Fatal
    deriving (Eq, Ord, Show, Generic)


instance ToJSON LogLevel where
    toEncoding lvl = toEncoding $ case lvl of
        Trace   -> "trace" :: T.Text
        Debug   -> "debug"
        Info    -> "info"
        Warning -> "warning"
        Error   -> "error"
        Fatal   -> "fatal"


shortFmt :: LogLevel -> T.Text
shortFmt lvl = case lvl of
    Trace   -> "TRAC "
    Debug   -> "DEBU "
    Info    -> "INFO "
    Warning -> "WARN "
    Error   -> "ERRO "
    Fatal   -> "FATA "


levelColor :: LogLevel -> Chunk -> Chunk
levelColor lvl = fore $ case lvl of
    Trace   -> white
    Debug   -> brightWhite
    Info    -> brightBlue
    Warning -> yellow
    Error   -> red
    Fatal   -> brightRed

data Field = forall a. LogEncodable a => Field !T.Text !a

data FieldMessage = FieldMessage
    { ns :: !T.Text
    , loglevel :: !LogLevel
    , message :: !T.Text
    , tags :: ![Field]
    , fields :: ![Field]
    }

data TimedMessage = TimedMessage
    { timestamp :: !Timestamp
    , msg :: !FieldMessage
    }


makeFieldLabelsNoPrefix  ''Field
makeFieldLabelsNoPrefix  ''FieldMessage
makeFieldLabelsNoPrefix  ''TimedMessage


class HasFields a where
    addField :: Field -> a -> a
    addTag :: Field -> a -> a

instance HasFields FieldMessage where
    addField :: Field -> FieldMessage -> FieldMessage
    addField field = over #fields (field:)

    addTag :: Field -> FieldMessage -> FieldMessage
    addTag tag = over #tags (tag:)

instance HasFields TimedMessage where
    addField :: Field -> TimedMessage -> TimedMessage
    addField field = over #msg (addField field)

    addTag :: Field -> TimedMessage -> TimedMessage
    addTag = over #msg . addTag

class HasNamespace a where
    namespace :: T.Text -> a -> a

instance HasNamespace FieldMessage where
    namespace = set #ns

instance HasNamespace TimedMessage where
    namespace = over #msg . set #ns


instance ToPrettyLog FieldMessage where
    prettyChunks FieldMessage{..} =
        let color = levelColor loglevel . chunk
            field (Field k v) =  color k :<| chunk "=" :<| prettyChunks v
            printFields [] = Seq.Empty
            printFields [x] = field x
            printFields (x:xs) = (field x :|> chunk " ") >< printFields xs
         in
            color (shortFmt loglevel)
            :<| chunk message
            :<| chunk (T.pack (replicate (30 - T.length message) ' '))
            :<| printFields fields
            >< printFields tags

instance ToJSON FieldMessage where
    toJSON FieldMessage{..} =
        object [ "namespace" .= ns
               , "loglevel"  .= loglevel
               , "message"   .= message
               , "fields"    .= object (map toPair fields)
               ]
        where
            toPair :: Field -> Pair
            toPair (Field k v) = (k, toJSON v)

    toEncoding FieldMessage{..} =
        pairs $ "namespace" .= ns
             <> "loglevel"  .= loglevel
             <> "message"   .= message
             <> fromFields tags
             <> pair "fields" (pairs $ fromFields fields)
        where
            fromFields = foldMap' (\(Field k v) -> k .= v)


instance ToPrettyLog TimedMessage where
    prettyChunks TimedMessage{..} =
        fore (colour256 248) (chunk $ formatTimestamp timestamp)
        :<| fore (colour256 248) (chunk " ")
        :<| prettyChunks msg


instance ToJSON TimedMessage where
    toJSON TimedMessage{..} = toJSON $ addTag (Field "timestamp" timestamp) msg
    toEncoding TimedMessage{..} = toEncoding $ addTag (Field "timestamp" timestamp) msg

newMsg :: T.Text -> LogLevel -> T.Text -> FieldMessage
newMsg ns lvl msg = FieldMessage ns lvl msg [] []

field :: (LogEncodable a, HasFields b) => T.Text -> a -> b -> b
field k v = addField $ Field k v

tag :: (LogEncodable a, HasFields b) => T.Text -> a -> b -> b
tag k v = addTag $ Field k v

fieldMaybe :: (LogEncodable a, HasFields b) => T.Text -> Maybe a -> b -> b
fieldMaybe _ Nothing = id
fieldMaybe k (Just v) = addField $ Field k v


(&) :: a -> (a -> b) -> b
(&) = (Data.Function.&)
infixl 1 &

printBuilderLn :: MonadIO m => Bldr.Builder -> m ()
printBuilderLn = liftIO . Bldr.hPutBuilder stdout . (<> "\n")


logPrettyStdout :: (ToPrettyLog a, MonadIO m) => TerminalCapabilities -> LogAction m a
logPrettyStdout cap =  LogAction (printBuilderLn . renderChunks cap . prettyChunks)


logStructuredStdout :: (ToJSON a, MonadIO m) => LogAction m a
logStructuredStdout = LogAction (printBuilderLn . fromEncoding . toEncoding)


timestampMessage :: MonadTime m => FieldMessage -> m TimedMessage
timestampMessage msg = flip TimedMessage msg <$> getCurrentTime

type WithLog env m = (MonadReader env m, HasLog env FieldMessage m)
type LogModifier = FieldMessage -> FieldMessage

type FieldLogger m = T.Text -> LogLevel -> T.Text -> LogModifier -> m ()
type MessageLogger m = T.Text -> LogLevel -> T.Text -> m ()

logText :: forall env m. WithLog env m => T.Text -> LogLevel -> T.Text -> m ()
logText ns lvl msg = logFields ns lvl msg id

logFields :: forall env m. WithLog env m => T.Text -> LogLevel -> T.Text -> LogModifier -> m ()
logFields ns lvl msg modify = do
    LogAction log <- asks getLogAction
    log . modify $ newMsg ns lvl msg


logInNamespace :: T.Text -> Q [Dec]
logInNamespace ns = [d|
        logMsg :: WithLog env m => LogLevel -> T.Text -> m ()
        logMsg = logText ns

        log' :: WithLog env m => LogLevel -> T.Text -> (FieldMessage -> FieldMessage) -> m ()
        log' = logFields ns
    |]
