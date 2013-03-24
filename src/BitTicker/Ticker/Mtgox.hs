{-# LANGUAGE TemplateHaskell #-}
module BitTicker.Ticker.Mtgox where

import Control.Applicative      ( (<$>), (<*>), pure)
import Control.Conditional      ( ifM)
import Control.Lens             ( makeLenses)
import Control.Monad            ( liftM, mzero)
import Data.Aeson               ( FromJSON(..), Value(..), (.:))
import Data.Text                ( Text)
import Data.Time.Clock          ( UTCTime)
import qualified Data.Sequence as S
import BitTicker.Fetch
import BitTicker.Util

-- all Mtgox JSON responses have a status and a result
data MtgoxResponse r
  = MtgoxResponse { _result :: r }
  | MtgoxError { _errorMsg :: Text }
  deriving (Eq, Show)

-- one kind of response is the ticker
data MtgoxTicker
  = MtgoxTicker
  { _high       :: MtgoxPrice
  , _low        :: MtgoxPrice
  , _avg        :: MtgoxPrice
  , _vwap       :: MtgoxPrice
  , _vol        :: MtgoxPrice
  , _last_local :: MtgoxPrice
  , _last       :: MtgoxPrice
  , _buy        :: MtgoxPrice
  , _sell       :: MtgoxPrice
  , _now        :: UTCTime
  } deriving (Eq, Show)

-- each ticker entry has these data
data MtgoxPrice
  = MtgoxPrice
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
makeLenses ''MtgoxPrice

-- implement JSON decoding instances for these data structures

instance FromJSON r => FromJSON (MtgoxResponse r) where
  parseJSON (Object v) =
    ifM (liftM (== success) $ v .: "result")
      (MtgoxResponse <$> v .: "data")
      (MtgoxError    <$> v .: "error")
      where success = "success" :: Text
  parseJSON _          = mzero

instance FromJSON MtgoxTicker where
  parseJSON (Object v) =
    MtgoxTicker <$> v .: "high" <*> v .: "low" <*> v .: "avg" <*>
                    v .: "vwap" <*> v .: "vol" <*>
                    v .: "last_local" <*> v .: "last" <*>
                    v .: "buy" <*> v .: "sell" <*>
                    (readMicroPosix <$> v .: "now")
  parseJSON _ = mzero

instance FromJSON MtgoxPrice where
  parseJSON (Object v) =
    MtgoxPrice <$> (read <$> v .: "value") <*>
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

isError :: MtgoxResponse r -> Bool
isError (MtgoxError _) = True
isError _              = False

getResponse :: MtgoxResponse r -> r
getResponse (MtgoxResponse r) = r
getResponse _                 = error "no response (error)"

fetchSample :: IO (Maybe Sample)
fetchSample = fetchDecode tickerURL
  where tickerURL = "https://data.mtgox.com/api/2/BTCUSD/money/ticker"

type History = S.Seq Sample
