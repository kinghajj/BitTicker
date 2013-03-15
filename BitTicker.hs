--------------------------------------------------------------------------------
-- BitTicker - A program to periodically fetch and display Bitcoin stats.     --
-- Copyright (C) 2013 Sam Fredrickson <kinghajj@gmail.com>                    --
--                                                                            --
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -O2 -rtsopts #-}
module Main where

import Control.Applicative      ( (<$>), (<*>), pure)
import Control.Concurrent       ( forkIO, threadDelay, MVar, newEmptyMVar
                                , tryPutMVar, tryTakeMVar)
import Control.Concurrent       ( runInUnboundThread)
import Control.Exception        ( IOException, catch)
import Control.Lens             ( makeLenses, view)
import Control.Monad            ( forever, mzero, void)
import Data.Aeson               ( FromJSON(..), Value(..), (.:), decode)
import Data.ByteString.Internal ( w2c)
import Data.ByteString.Lazy     ( ByteString, hGetContents, unpack)
import Data.Foldable            ( toList)
import Data.Maybe               ( isJust)
import Data.Sequence            ( (<|))
import Data.Text                ( Text)
import Data.Time.Clock          ( UTCTime)
import Data.Time.Clock.POSIX    ( posixSecondsToUTCTime)
import GHC.Float                ( double2Float)
import Prelude hiding           ( catch)
import System.Console.CmdArgs   ( Data, Typeable, (&=), cmdArgs, help, summary)
import System.IO                ( hSetBuffering, BufferMode(..), stdout)
import System.Locale            ( defaultTimeLocale)
import System.Process           ( CreateProcess(..), StdStream(..), CmdSpec(..)
                                , createProcess)
import System.Time              ( formatCalendarTime, getClockTime
                                , toCalendarTime)
import Text.Printf              ( printf)
import qualified Data.Sequence as S

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate

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

readMicroPosix :: String -> UTCTime
readMicroPosix pt =
  posixSecondsToUTCTime $ fromInteger $ read pt `div` 1000000

--------------------------------------------------------------------------------
-- Ticker data structures

-- all Mtgox JSON responses have a status and a result
data MtgoxResponse r =
  MtgoxResponse { _status :: Text, _result :: r } deriving (Eq, Show)

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
  , _now        :: UTCTime
  } deriving (Eq, Show)

-- each ticker entry has these data
data MtgoxStats
  = MtgoxStats
  { _value         :: Double
  , _value_int     :: Integer
  , _text          :: Text
  , _text_short    :: Text
  , _currency      :: Currency
  } deriving (Eq, Show)

-- the three currencies we're concerned with
data Currency = USD | EUR | BTC deriving (Eq, Read, Show)

-- make convenient lenses for these data structures
makeLenses ''MtgoxResponse
makeLenses ''MtgoxTicker
makeLenses ''MtgoxStats

-- implement JSON decoding instances for these data structures

instance FromJSON r => FromJSON (MtgoxResponse r) where
  parseJSON (Object v) = MtgoxResponse <$> v .: "result" <*> v .: "data"
  parseJSON _          = mzero

instance FromJSON MtgoxTicker where
  parseJSON (Object v) =
    MtgoxTicker <$> v .: "high" <*> v .: "low" <*> v .: "avg" <*>
                    v .: "vwap" <*> v .: "vol" <*>
                    v .: "last_local" <*> v .: "last" <*>
                    v .: "buy" <*> v .: "sell" <*>
                    (readMicroPosix <$> v .: "now")
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

-- the core sample stored by the program
type Sample = MtgoxResponse MtgoxTicker

--------------------------------------------------------------------------------
-- Info fetching

fetchDecode :: FromJSON a => String -> IO (Maybe a)
fetchDecode = runInUnboundThread . fmap decode . fetchHTTP

fetchSample :: IO (Maybe Sample)
fetchSample = fetchDecode tickerURL
  where tickerURL = "https://data.mtgox.com/api/2/BTCUSD/money/ticker"

--------------------------------------------------------------------------------
-- Main program

newtype Timer = Timer (MVar ())

newTimer :: IO Timer
newTimer = Timer <$> newEmptyMVar

tick :: Timer -> IO Bool
tick (Timer m) = tryPutMVar m ()

ticked :: Timer -> IO Bool
ticked (Timer m) = isJust <$> tryTakeMVar m

drawSamples :: [Sample] -> Picture
drawSamples samples =
  let rsamples  = reverse samples
      priceline = view value . view last_local
      buyline   = view value . view buy
      sellline  = view value . view sell
      sep       = [0,0.1..]
      zipP  f   = zip sep $ map (double2Float . f . view result) rsamples
      makeL     = line . zipP
      makeC p   = uncurry translate p $ circle 0.05
      makeS     = pictures . map makeC . zipP
  in pictures [ (color green  $ makeL priceline)
              , (color green  $ makeS priceline)
              , (color orange $ makeL buyline)
              , (color cyan   $ makeL sellline)
              ]

displaySamples :: S.Seq Sample -> IO Picture
displaySamples samples =
  pure $ pictures [drawSamples $ toList samples, haxis, vaxis, tens]
  where haxis = color white $ line [(-100,0), (100,0)]
        vaxis = color white $ line [(0,-100), (0,100)]
        tens  = color white $ pictures $ map line $
                foldr (\n ps -> [(-1,n),(1,n)]:ps) [] [10,20..100]

updateSamples :: Timer -> S.Seq Sample -> IO (S.Seq Sample)
updateSamples timer samples = do
  ding <- ticked timer
  if S.null samples || ding
    then fetchSample >>= \ms ->
         pure $ case ms of
           Just s  -> s <| samples
           Nothing -> samples
    else pure samples

-- command line options
data Cfg = Cfg { delay :: Int }
         deriving (Show, Data, Typeable)

cmdCfg :: Cfg
cmdCfg =
  Cfg
  { delay = 90000000 &= help "sample delay in microseconds"
  }
  &= summary "Personal Bitcoin Ticker"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  cfg   <- cmdArgs cmdCfg
  timer <- newTimer
  -- periodically tick the timer
  void $ forkIO $ every (delay cfg) $ void $ tick timer
  -- display it
  simulateIO (InWindow "BitTicker" (800,600) (100,100)) black 1 S.empty
             displaySamples (\_ _ -> updateSamples timer)
