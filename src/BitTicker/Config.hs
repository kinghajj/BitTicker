{-# LANGUAGE DeriveDataTypeable #-}
module BitTicker.Config where

import System.Console.CmdArgs   ( Data, Typeable, (&=), help, summary)

-- command line options
data Cfg = Cfg { delay :: Int }
         deriving (Show, Data, Typeable)

cmdCfg :: Cfg
cmdCfg =
  Cfg
  { delay = 90000000 &= help "sample delay in microseconds"
  }
  &= summary "Personal Bitcoin Ticker"
