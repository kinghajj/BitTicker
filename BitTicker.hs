--------------------------------------------------------------------------------
-- BitTicker - A program to periodically fetch and display Bitcoin stats.     --
-- Copyright (C) 2013 Sam Fredrickson <kinghajj@gmail.com>                    --
--                                                                            --
--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -O2 -rtsopts #-}
module Main where

import Control.Applicative    ( (<$>), (<*>))
import Control.Concurrent     ( forkIO, threadDelay)
import Control.Exception      ( IOException, catch)
import Control.Monad          ( forever, void)
import Network.HTTP           ( getRequest, getResponseBody, simpleHTTP)
import Prelude hiding         ( catch)
import System.Console.CmdArgs ( Data, Typeable, (&=), cmdArgs, help, summary)
import System.IO              ( BufferMode(..), hSetBuffering, stdout)
import System.Locale          ( defaultTimeLocale)
import System.Time            ( formatCalendarTime, getClockTime
                              , toCalendarTime)
import Text.Printf            ( printf)

-- number of BTC allocated per second, assuming 25 BTC per 10 minutes
btcPerSec :: Double
btcPerSec = 0.04167

-- the hashes per second of a hypothetical mining rig
myRate :: Double
myRate = 30e9

-- output a string with an ISO8601 timestamp
timedLog :: String -> IO ()
timedLog s = getClockTime >>= toCalendarTime >>= \t -> printf "%s: %s" (iso t) s
  where iso = formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- fetch an HTTP resource and return the body
getHTTP :: String -> IO String
getHTTP url = getResponseBody =<< simpleHTTP (getRequest url)

-- fetch an HTTP resource and convert to a double
getHTTPDouble :: String -> IO Double
getHTTPDouble = (read <$>) . getHTTP

newtype BTCPrice = BTCPrice Double
newtype NetHashRate = NetHashRate Double

data Results = Results { btcPrice :: BTCPrice, netHashRate :: NetHashRate }

-- fetch the price of BTC
getPriceOfBTC :: IO BTCPrice
getPriceOfBTC = BTCPrice . (**(-1)) <$> getHTTPDouble url
  where url = "http://blockchain.info/tobtc?currency=USD&value=1"

-- fetch the network hash rate
getNetHashRate :: IO NetHashRate
getNetHashRate = fmap NetHashRate $
                 (/) <$> getHTTPDouble "http://blockexplorer.com/q/hashestowin"
                     <*> getHTTPDouble "http://blockexplorer.com/q/interval/144"

-- compute weekly mining income given a current network rate
weeklyMiningIncome :: Double -> Double
weeklyMiningIncome hr = (myRate/(hr+myRate)) * btcPerSec * 60 * 60 * 24 * 7

-- fetch and bundle the BTC price and network hash rate
fetch :: IO Results
fetch = timedLog "fetching\r" >> Results <$> getPriceOfBTC <*> getNetHashRate

-- display the fetched results
display :: Results -> IO ()
display results =
  let (BTCPrice p)     = btcPrice results
      (NetHashRate hr) = netHashRate results
      weeklyBTC        = weeklyMiningIncome hr
      weeklyUSD        = weeklyBTC * p
      fmt              = "$%.5f (%.5f/$%.5f weekly)\n"
  in  timedLog $ printf fmt p weeklyBTC weeklyUSD

-- repeat some action at a given interval
every :: Int -> IO () -> IO ()
every interval action = forever $ do
  void $ forkIO $ catch action $ \(e :: IOException) ->
    timedLog $ "error: " ++ show e
  threadDelay interval

-- command line options
data Cfg = Cfg { delay :: Int } deriving (Show, Data, Typeable)

cmdCfg :: Cfg
cmdCfg = Cfg { delay = 60000000 &= help "delay"}
         &= summary "Personal Bitcoin Ticker"

main :: IO ()
main = hSetBuffering stdout NoBuffering >> (main' =<< cmdArgs cmdCfg)
  where main' cfg = every (delay cfg) $ fetch >>= display
