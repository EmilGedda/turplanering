{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Turplanering.Time
    ( module Data.Time.Clock
    , module C
    , C.zonedTimeToUTC
    , MonadTime (..)
    , diffTimestamp
    , formatTimestamp
    , formatTime
    , Timestamp
    ) where

import           Control.Monad.Trans        (MonadTrans, lift)
import           Data.Time.Clock            hiding (getCurrentTime)
import           Data.Time.LocalTime        (utcToLocalTime, LocalTime, getCurrentTimeZone)
import qualified Control.Monad.State.Strict as Strict
import qualified Data.Time                  as C
import qualified Data.Time.Format as Fmt
import qualified Data.Text as T


type Timestamp = C.ZonedTime
type Trans c m a = forall m1 t. (MonadTrans t, c m1, m ~ t m1) => m a

class Monad m => MonadTime m where
    getCurrentTime :: m Timestamp
    default getCurrentTime :: Trans MonadTime m Timestamp
    getCurrentTime = lift getCurrentTime


instance MonadTime IO where
    getCurrentTime = C.getZonedTime


instance MonadTime m => MonadTime (Strict.StateT s m)

diffTimestamp :: Timestamp -> Timestamp -> NominalDiffTime
diffTimestamp a b = diffUTCTime (C.zonedTimeToUTC a) (C.zonedTimeToUTC b)

formatTimestamp :: Timestamp -> T.Text
formatTimestamp = T.pack . Fmt.formatTime Fmt.defaultTimeLocale "%X"

formatTime :: C.FormatTime s => String -> s -> T.Text
formatTime format time = T.pack $ C.formatTime C.defaultTimeLocale format time
