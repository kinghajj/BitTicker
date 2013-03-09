--------------------------------------------------------------------------------
-- BitTicker - A program to periodically fetch and display Bitcoin stats.     --
-- Copyright (C) 2013 Sam Fredrickson <kinghajj@gmail.com>                    --
--                                                                            --
--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -O2 -rtsopts #-}
module Main where

import Control.Applicative      ( (<$>), (<*>), pure, liftA2)
import Control.Concurrent       ( forkIO, threadDelay
                                , MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Conditional      ( (?), (??))
import Control.Exception        ( IOException, catch)
import Control.Lens             ( (^.), makeLenses)
import Control.Monad            ( forever, mzero, void, when)
import Data.Aeson               ( FromJSON(..), Value(..), (.:), decode)
import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy     ( ByteString, hGetContents, unpack)
import Data.Maybe               ( fromJust)
import Data.Text                ( Text)
import Prelude hiding           ( catch)
import System.Console.CmdArgs   ( Data, Typeable, (&=), cmdArgs, help, summary)
import System.IO                ( BufferMode(..), hSetBuffering, stdout)
import System.Locale            ( defaultTimeLocale)
import System.Process           ( CreateProcess(..), StdStream(..), CmdSpec(..)
                                , createProcess, system)
import System.Time              ( formatCalendarTime, getClockTime
                                , toCalendarTime)
import Text.Printf              ( printf)

--------------------------------------------------------------------------------
-- Configuration

-- the sounds to play on significant price changes
upSound, downSound :: String
upSound   = "C:\\Windows\\Media\\tada.wav"
downSound = "C:\\Windows\\Media\\chord.wav"

--------------------------------------------------------------------------------
-- Utility functions

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

-- use 'wget' to fetch an HTTP object
fetchHTTP :: String -> IO ByteString
fetchHTTP url = createProcess wget >>= \(_, mstdout, _, _) ->
                case mstdout of
                  Just h  -> hGetContents h
                  Nothing -> error "wget had no stdout handle???"
  where cmd  = "wget --no-check-certificate -q -O - \"%s\""
        wget = CreateProcess (ShellCommand $ printf cmd url)
                             Nothing Nothing CreatePipe CreatePipe CreatePipe
                             False False

-- fetch an HTTP object an attempt to read it
fetchRead :: Read a => String -> IO a
fetchRead = fmap (read . map w2c . unpack) . fetchHTTP

-- use powershell to play a sound file
playSound :: String -> IO ()
playSound = void . forkIO . void . system . printf fmt
  where fmt = "powershell -c (New-Object Media.SoundPlayer \"%s\").PlaySync();"

--------------------------------------------------------------------------------
-- Ticker data structures

-- all Mtgox JSON responses have a status and a result
data MtgoxResponse r =
  MtgoxResponse { _status :: Text, _result :: r } deriving Show

-- one kind of response is the ticker
data MtgoxTicker
  = MtgoxTicker
  { _high       :: MtgoxStats
  , _low        :: MtgoxStats
  , _avg        :: MtgoxStats
  , _vwap       :: MtgoxStats
  , _vol        :: MtgoxStats
  , _last_local :: MtgoxStats
  , _last       :: MtgoxStats
  , _buy        :: MtgoxStats
  , _sell       :: MtgoxStats
  } deriving Show

-- each ticker entry has these data
data MtgoxStats
  = MtgoxStats
  { _value         :: Double
  , _value_int     :: Integer
  , _text          :: Text
  , _text_short    :: Text
  , _currency      :: Currency
  } deriving Show

-- the three currencies we're concerned with
data Currency = USD | EUR | BTC deriving (Read, Show)

-- another response is the lag counter
data MtgoxLag
  = MtgoxLag
  { _lag      :: Integer
  , _lag_secs :: Double
  , _lag_text :: Text
  } deriving Show

-- make convenient lenses for these data structures
makeLenses ''MtgoxResponse
makeLenses ''MtgoxTicker
makeLenses ''MtgoxStats
makeLenses ''MtgoxLag

-- implement JSON decoding instances for these data structures

instance FromJSON r => FromJSON (MtgoxResponse r) where
  parseJSON (Object v) = MtgoxResponse <$> v .: "result" <*> v .: "return"
  parseJSON _          = mzero

instance FromJSON MtgoxTicker where
  parseJSON (Object v) =
    MtgoxTicker <$> v .: "high" <*> v .: "low" <*> v .: "avg" <*>
                    v .: "vwap" <*> v .: "vol" <*>
                    v .: "last_local" <*> v .: "last" <*>
                    v .: "buy" <*> v .: "sell"
  parseJSON _ = mzero

instance FromJSON MtgoxStats where
  parseJSON (Object v) =
    MtgoxStats <$> (read <$> v .: "value") <*>
                   (read <$> v .: "value_int") <*>
                   v .: "display" <*>
                   v .: "display_short" <*>
                   v .: "currency"
  parseJSON _ = mzero

instance FromJSON Currency where
  parseJSON (String "USD") = pure USD
  parseJSON (String "EUR") = pure EUR
  parseJSON (String "BTC") = pure BTC
  parseJSON _              = mzero

instance FromJSON MtgoxLag where
  parseJSON (Object v) =
    MtgoxLag <$> v .: "lag" <*> v .: "lag_secs" <*> v .: "lag_text"
  parseJSON _ = mzero

-- We also sample the network hash rate from another source
newtype NetHashRate = NetHashRate Double deriving (Eq, Show)

-- the core sample stored by the program
data Sample
  = Sample
  { ticker      :: MtgoxResponse MtgoxTicker
  , lagger      :: MtgoxResponse MtgoxLag
  }

--------------------------------------------------------------------------------
-- Info fetching

fetchDecode :: FromJSON a => String -> IO a
fetchDecode = fmap (fromJust . decode) . fetchHTTP

fetchMtgoxTicker :: IO (MtgoxResponse MtgoxTicker)
fetchMtgoxTicker = fetchDecode "https://mtgox.com/api/1/BTCUSD/ticker"

fetchMtgoxLag :: IO (MtgoxResponse MtgoxLag)
fetchMtgoxLag = fetchDecode "https://mtgox.com/api/1/generic/order/lag"

--------------------------------------------------------------------------------
-- Main program

-- fetch and bundle the BTC price and network hash rate
fetch :: IO Sample
fetch = timedLog "fetching\r" >>
        liftA2 Sample fetchMtgoxTicker fetchMtgoxLag

-- display the fetched Sample
display :: MVar Sample -> Cfg -> Sample -> IO ()
display prevsample cfg sample = do
  -- extract/calculate data to display
  let curticker        = (ticker sample)^.result
  let curlagger        = (lagger sample)^.result
  let curlag           = curlagger^.lag_secs
  let curlast_all      = curticker^.last_local
  let curprice         = curlast_all^.value
  let curbuy           = curticker^.buy^.value
  let cursell          = curticker^.sell^.value
  let curhigh          = curticker^.high^.value
  let curlow           = curticker^.low^.value
  -- display it!
  timedLog $ (printf "$%.2f $%.2f-$%.2f L$%.2f H$%.2f %.2fs\n"
                     curprice curbuy cursell curlow
                     curhigh curlag :: String)
  -- if there was a previous sample, maybe play a sound
  mpsample <- tryTakeMVar prevsample
  case mpsample of
    (Just psample) -> do
      let prevticker = (ticker psample)^.result
      let prevprice  = prevticker^.last_local^.value
      let lastbuy    = prevticker^.buy^.value
      let lastsell   = prevticker^.sell^.value
      let changed    = curprice /= prevprice
      let outside    = curprice < lastbuy || curprice > lastsell
      -- if price changed and outside previous sample's spread, play sound
      when (playSounds cfg && changed && outside) $
        playSound $ curprice >= prevprice ? upSound ?? downSound
    _ -> pure ()
  -- remember this sample for next time
  putMVar prevsample sample

-- command line options
data Cfg = Cfg { delay :: Int, playSounds :: Bool}
         deriving (Show, Data, Typeable)

cmdCfg :: Cfg
cmdCfg =
  Cfg
  { delay = 15000000   &= help "sample delay in microseconds"
  , playSounds = False &= help "play sounds?"
  }
  &= summary "Personal Bitcoin Ticker"

main :: IO ()
main = hSetBuffering stdout NoBuffering >> (main' =<< cmdArgs cmdCfg)
  where main' cfg = do psample <- newEmptyMVar
                       every (delay cfg) $ fetch >>= display psample cfg
