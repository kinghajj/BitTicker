module BitTicker.Text where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Time
import Data.Time.Clock.POSIX
import Text.Printf
import BitTicker.Config
import BitTicker.Ticker.Mtgox
import BitTicker.Util

printTicker :: MtgoxTicker -> IO ()
printTicker t = do
  curTime <- utcTimeToPOSIXSeconds <$> getCurrentTime
  let curPrice = t^.last_local^.value
  --printf "0:%s\n" (show $ round curTime)
  printf "0:%f\n" curPrice

launchText :: Cfg -> IO ()
launchText cfg = every (delay cfg) $ fetchSample >>= \case
  (Just s) -> when (not $ isError s) $ printTicker $ getResponse s
  Nothing  -> pure ()
