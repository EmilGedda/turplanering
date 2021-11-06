{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Turplanering.Time (module Data.Time.Clock, MonadTime (..)) where

import           Control.Monad.Trans        (MonadTrans, lift)
import           Data.Time.Clock            hiding (getCurrentTime)
import qualified Control.Monad.State.Strict as Strict
import qualified Data.Time.Clock            as C


type Trans c m a = forall m1 t. (MonadTrans t, c m1, m ~ t m1) => m a


class Monad m => MonadTime m where
    getCurrentTime :: m C.UTCTime
    default getCurrentTime :: Trans MonadTime m C.UTCTime
    getCurrentTime = lift getCurrentTime


instance MonadTime IO where
    getCurrentTime = C.getCurrentTime


instance MonadTime m => MonadTime (Strict.StateT s m)
