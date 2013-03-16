--------------------------------------------------------------------------------
-- BitTicker - A program to periodically fetch and display Bitcoin stats.     --
-- Copyright (C) 2013 Sam Fredrickson <kinghajj@gmail.com>                    --
--                                                                            --
--------------------------------------------------------------------------------

module Main where

import System.IO                ( hSetBuffering, BufferMode(..), stdout)
import System.Console.CmdArgs   ( cmdArgs)
import BitTicker.Config         ( cmdCfg)
import BitTicker.UI             ( launchUI)

main :: IO ()
main = hSetBuffering stdout NoBuffering >> cmdArgs cmdCfg >>= launchUI
