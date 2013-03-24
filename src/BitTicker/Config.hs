{-# LANGUAGE DeriveDataTypeable #-}
module BitTicker.Config where

import System.Console.CmdArgs   ( Data, Typeable, (&=), help, summary)

-- command line options
data Cfg = Cfg { delay :: Int, gui :: Bool }
         deriving (Show, Data, Typeable)

cmdCfg :: Cfg
cmdCfg =
  Cfg
  { delay = 90000000 &= help "sample delay in microseconds"
  , gui = False      &= help "whether to use the gui"
  }
  &= summary "Personal Bitcoin Ticker"
