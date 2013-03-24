module BitTicker.Text where

import Control.Applicative (pure)
import Control.Lens        ((^.))
import Control.Monad       (when)
import Text.Printf         (printf)

import BitTicker.Config
import BitTicker.Ticker.Mtgox
import BitTicker.Util

printTicker :: MtgoxTicker -> IO ()
printTicker t = printf "0:%f\n" $ t^.last_local^.value

launchText :: Cfg -> IO ()
launchText cfg = every (delay cfg) $ fetchSample >>= \case
  (Just s) -> when (not $ isError s) $ printTicker $ getResponse s
  Nothing  -> pure ()
