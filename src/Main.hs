--------------------------------------------------------------------------------
-- BitTicker - A program to periodically fetch and display Bitcoin stats.     --
-- Copyright (C) 2013 Sam Fredrickson <kinghajj@gmail.com>                    --
--                                                                            --
--------------------------------------------------------------------------------

module Main where

import System.IO                ( hSetBuffering, BufferMode(..), stdout)
import System.Console.CmdArgs   ( cmdArgs)
import BitTicker.Config         ( Cfg(..), cmdCfg)
import BitTicker.Text           ( launchText)
import BitTicker.UI             ( launchUI)

chooseLauncher :: Cfg -> IO ()
chooseLauncher cfg = (if gui cfg then launchUI else launchText) cfg

main :: IO ()
main = hSetBuffering stdout NoBuffering >> cmdArgs cmdCfg >>= chooseLauncher
