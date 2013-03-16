module BitTicker.UI (launchUI) where

import Control.Applicative      ( pure)
import Control.Concurrent       ( forkIO)
import Control.Lens             ( view)
import Control.Monad            ( void)
import GHC.Float                ( double2Float)
import Data.Foldable            ( toList)
import Data.Sequence            ( (<|))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import qualified Data.Sequence as S

import BitTicker.Config
import BitTicker.Ticker.Mtgox
import BitTicker.Timer
import BitTicker.Util

drawTickers :: [MtgoxTicker] -> Picture
drawTickers samples =
  let rsamples  = reverse samples
      -- evenly space the samples at 0.1 unit intervals
      sep = [0,0.1..]
      -- take values from ticker and zip with separation intervals
      zipP f = zip sep $ map (double2Float . f) rsamples
      -- make a line from ticker values
      makeL = line . zipP
      -- make a circle at a given point
      makeC p = uncurry translate p $ circle 0.05
      -- make a scatter plot of circles from ticker values
      makeS = pictures . map makeC . zipP
      -- various interesting info to track
      prices = view value . view last_local
      buys   = view value . view buy
      sells  = view value . view sell
  in pictures [ color green  $ makeL prices
              , color green  $ makeS prices
              , color orange $ makeL buys
              , color cyan   $ makeL sells
              ]

displaySamples :: S.Seq Sample -> Picture
displaySamples samples = pictures [drawTickers ticks, haxis, vaxis, tens]
  where ticks = map getResponse $ filter (not . isError) $ toList samples
        haxis = color white $ line [(-100,0), (100,0)]
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

launchUI :: Cfg -> IO ()
launchUI cfg = do
  timer <- newTimer
  -- periodically tick the timer
  void $ forkIO $ every (delay cfg) $ void $ tick timer
  -- display it
  simulateIO (InWindow "BitTicker" (800,600) (100,100)) black 1 S.empty
             (pure . displaySamples) (\_ _ -> updateSamples timer)
