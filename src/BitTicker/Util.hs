module BitTicker.Util where

import Control.Concurrent       ( forkIO, threadDelay)
import Control.Exception        ( IOException, catch)
import Control.Monad            ( forever, void)
import Data.Time.Clock          ( UTCTime)
import Data.Time.Clock.POSIX    ( posixSecondsToUTCTime)
import Prelude hiding           ( catch)
import System.Locale            ( defaultTimeLocale)
import System.Time              ( formatCalendarTime, getClockTime
                                , toCalendarTime)
import Text.Printf              ( printf)

-- output a string with an ISO8601 timestamp
timedLog :: String -> IO ()
timedLog s = getClockTime >>= toCalendarTime >>= \t -> printf "%s: %s" (iso t) s
  where iso = formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- repeat some action at a given interval
every :: Int -> IO () -> IO ()
every interval action = forever $ do
  void $ forkIO $ catch action $ \(e :: IOException) ->
    timedLog $ printf "error: %s" $ show e
  threadDelay interval

readMicroPosix :: String -> UTCTime
readMicroPosix pt =
  posixSecondsToUTCTime $ fromInteger $ read pt `div` 1000000
