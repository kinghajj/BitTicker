module BitTicker.Timer (Timer, newTimer, tick, ticked) where

import Control.Applicative ((<$>))
import Data.Maybe          ( isJust)
import Control.Concurrent  ( MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)

newtype Timer = Timer (MVar ())

newTimer :: IO Timer
newTimer = Timer <$> newEmptyMVar

-- "tick" attempts to set the timer alert, returning true on success
-- "ticked" attempts to remove the alert status, returning true on success
tick, ticked :: Timer -> IO Bool
tick   (Timer m) = tryPutMVar m ()
ticked (Timer m) = isJust <$> tryTakeMVar m
