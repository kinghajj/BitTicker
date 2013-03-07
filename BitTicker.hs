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

import Control.Applicative      ( (<$>), (<*>), pure)
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

-- number of BTC allocated per second, assuming 25 BTC per 10 minutes
btcPerSec :: Double
btcPerSec = 0.04167

-- the hashes per second of a hypothetical mining rig
myRate :: Double
myRate = 30e9

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
readHTTP :: Read a => String -> IO a
readHTTP = fmap (read . map w2c . unpack) . fetchHTTP

-- use powershell to play a sound file
playSound :: String -> IO ()
playSound = void . forkIO . void . system . printf fmt
  where fmt = "powershell -c (New-Object Media.SoundPlayer \"%s\").PlaySync();"

--------------------------------------------------------------------------------
-- Ticker data structures

-- data structures for the MtGox HTTP API v1
data MtgoxTicker
  = MtgoxTicker
  { _tickerstatus :: Text
  , _tickerresult :: MtgoxTickerResult
  } deriving Show

data MtgoxTickerResult
  = MtgoxTickerResult
  { _high       :: MtgoxStats
  , _low        :: MtgoxStats
  , _avg        :: MtgoxStats
  , _vwap       :: MtgoxStats
  , _vol        :: MtgoxStats
  , _last_local :: MtgoxStats
  , _last_all   :: MtgoxStats
  , _last       :: MtgoxStats
  , _buy        :: MtgoxStats
  , _sell       :: MtgoxStats
  } deriving Show

data MtgoxStats
  = MtgoxStats
  { _value         :: Double
  , _value_int     :: Integer
  , _text          :: Text
  , _text_short    :: Text
  , _currency      :: Currency
  } deriving Show

data Currency = USD | EUR | BTC deriving (Read, Show)

data MtgoxLag
  = MtgoxLag
  { _lagstatus :: Text
  , _lagresult :: MtgoxLagResult
  } deriving Show

data MtgoxLagResult
  = MtgoxLagResult
  { _lag      :: Integer
  , _lag_secs :: Double
  , _lag_text :: Text
  } deriving Show

makeLenses ''MtgoxTicker
makeLenses ''MtgoxTickerResult
makeLenses ''MtgoxStats
makeLenses ''MtgoxLag
makeLenses ''MtgoxLagResult

-- instances to convert JSON object
instance FromJSON MtgoxTicker where
  parseJSON (Object v) = MtgoxTicker <$> v .: "result" <*> v .: "return"
  parseJSON _          = mzero

instance FromJSON MtgoxTickerResult where
  parseJSON (Object v) = MtgoxTickerResult <$>
                         v .: "high"       <*>
                         v .: "low"        <*>
                         v .: "avg"        <*>
                         v .: "vwap"       <*>
                         v .: "vol"        <*>
                         v .: "last_local" <*>
                         v .: "last_all"   <*>
                         v .: "last"       <*>
                         v .: "buy"        <*>
                         v .: "sell"
  parseJSON _ = mzero

instance FromJSON MtgoxStats where
  parseJSON (Object v) = MtgoxStats                  <$>
                         (read <$> v .: "value")     <*>
                         (read <$> v .: "value_int") <*>
                         v .: "display"              <*>
                         v .: "display_short"        <*>
                         v .: "currency"
  parseJSON _ = mzero

instance FromJSON Currency where
  parseJSON (String "USD") = pure USD
  parseJSON (String "EUR") = pure EUR
  parseJSON (String "BTC") = pure BTC
  parseJSON _              = mzero

instance FromJSON MtgoxLag where
  parseJSON (Object v) = MtgoxLag <$> v .: "result" <*> v .: "return"
  parseJSON _          = mzero

instance FromJSON MtgoxLagResult where
  parseJSON (Object v) = MtgoxLagResult  <$>
                         v .: "lag"      <*>
                         v .: "lag_secs" <*>
                         v .: "lag_text"
  parseJSON _ = mzero

newtype NetHashRate = NetHashRate Double deriving (Eq, Show)

-- the core results stored by the program
data Results
  = Results
  { ticker      :: MtgoxTicker
  , lagger      :: MtgoxLag
  , netHashRate :: NetHashRate
  }

--------------------------------------------------------------------------------
-- Ticker info fetching

fetchDecode :: FromJSON a => String -> IO a
fetchDecode = fmap fromJust . fmap decode . fetchHTTP

fetchMtgoxTicker :: IO MtgoxTicker
fetchMtgoxTicker = fetchDecode "https://mtgox.com/api/1/BTCUSD/ticker"

fetchMtgoxLag :: IO MtgoxLag
fetchMtgoxLag = fetchDecode "https://mtgox.com/api/1/generic/order/lag"

-- fetch the network hash rate
fetchNetHashRate :: IO NetHashRate
fetchNetHashRate = fmap NetHashRate $
                   (/) <$> readHTTP "http://blockexplorer.com/q/hashestowin"
                       <*> readHTTP "http://blockexplorer.com/q/interval/144"

--------------------------------------------------------------------------------
-- Addition info calculations

-- compute weekly mining income given a current network rate
weeklyMiningIncome :: Double -> Double
weeklyMiningIncome hr = (myRate/(hr+myRate)) * btcPerSec * 60 * 60 * 24 * 7

--------------------------------------------------------------------------------
-- Main program

-- fetch and bundle the BTC price and network hash rate
fetch :: IO Results
fetch = timedLog "fetching\r" >>
        Results <$> fetchMtgoxTicker <*> fetchMtgoxLag <*> fetchNetHashRate

-- display the fetched results
display :: MVar Results -> Cfg -> Results -> IO ()
display prev cfg results = do
  mpresults <- tryTakeMVar prev
  let curticker        = (ticker results)^.tickerresult
  let curlagger        = (lagger results)^.lagresult
  let curlag           = curlagger^.lag_secs
  let curlast_all      = curticker^.last_local
  let curprice         = curlast_all^.value
  let curbuy           = curticker^.buy^.value
  let cursell          = curticker^.sell^.value
  let curhigh          = curticker^.high^.value
  let curlow           = curticker^.low^.value
  let (NetHashRate hr) = netHashRate results   -- then calc new ones
  let wBTC             = weeklyMiningIncome hr -- weekly BTC income
  timedLog $ (printf "$%.2f $%.2f-$%.2f L$%.2f H$%.2f %.2fs %.2f\n"
                     curprice curbuy cursell curlow
                     curhigh curlag wBTC :: String)
  case mpresults of
    (Just presults) -> do
      let prevticker = (ticker presults)^.tickerresult
      let prevprice  = prevticker^.last_local^.value
      let lastbuy    = prevticker^.buy^.value
      let lastsell   = prevticker^.sell^.value
      let changed    = curprice /= prevprice
      let outside    = curprice < lastbuy || curprice > lastsell
      when (playSounds cfg && changed && outside) $
        playSound $ curprice >= prevprice ? upSound ?? downSound
    _ -> pure ()
  putMVar prev results

-- command line options
data Cfg = Cfg { delay :: Int, playSounds :: Bool}
         deriving (Show, Data, Typeable)

cmdCfg :: Cfg
cmdCfg = Cfg { delay = 15000000   &= help "delay"
             , playSounds = False &= help "play sounds"}
         &= summary "Personal Bitcoin Ticker"

main :: IO ()
main = hSetBuffering stdout NoBuffering >> (main' =<< cmdArgs cmdCfg)
  where main' cfg = do presults <- newEmptyMVar
                       every (delay cfg) $ fetch >>= display presults cfg
