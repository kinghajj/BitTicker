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

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  cfg <- cmdArgs cmdCfg
  let launcher = if (gui cfg) then launchUI else launchText
  launcher cfg
