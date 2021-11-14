module Turplanering.Log.Fields ((Turplanering.Log.Fields.>>)) where

(>>) :: (b -> c) -> (a -> b) -> a -> c
(>>) = (.)
