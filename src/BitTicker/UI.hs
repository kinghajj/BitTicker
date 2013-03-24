module BitTicker.UI (launchUI) where

import Control.Arrow            ( (>>>))
import Control.Concurrent       ( MVar, forkIO, newMVar, putMVar, readMVar,
                                  takeMVar)
import Control.Lens             ( view)
import Control.Monad            ( void)
import Control.Monad.Trans      ( liftIO)
import Data.Foldable            ( toList)
import Graphics.UI.Gtk
import Diagrams.Backend.Cairo
import Diagrams.Backend.Gtk
import Diagrams.Prelude hiding (value, view)

import qualified Data.Sequence as S

import BitTicker.Config
import BitTicker.Ticker.Mtgox
import BitTicker.Util

renderHistory :: History -> Diagram Cairo R2
renderHistory h =
  renderLine last_local green <> renderLine buy orange <> renderLine sell blue
  where resps = toList >>> filter (not . isError) >>> map getResponse $ h
        renderLine f clr = map (view f) >>> map (view value) >>> zip [1..] >>>
                           map p2 >>> map (flip (,) $ unitCircle # fc clr) >>>
                           position $ resps

renderDiagram :: History -> Diagram Cairo R2
renderDiagram history = renderHistory history <> rect 700 500 # fc black

renderMV :: MVar History -> EventM EExpose Bool
renderMV historymv = do
  history <- liftIO $ readMVar historymv
  window  <- eventWindow
  liftIO $ renderToGtk window $ toGtkCoords $ renderDiagram history
  pure True

launchUI :: Cfg -> IO ()
launchUI cfg = do
  -- create mvar to store history of samples
  historymv <- newMVar S.empty
  -- initialize the Gtk UI
  void initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  void $ canvas `on` sizeRequest $ pure (Requisition 800 600)
  set window [ containerBorderWidth := 10, containerChild := canvas ]
  void $ canvas `on` exposeEvent $ renderMV historymv
  void $ onDestroy window mainQuit
  -- periodically fetch new sample and refresh UI
  void $ forkIO $ every (delay cfg) $ do
    history <- takeMVar historymv
    fetchSample >>= \case
      (Just s) -> putMVar historymv $ (S.|>) history s
      Nothing  -> putMVar historymv history
    widgetQueueDraw canvas
  -- show it
  widgetShowAll window
  mainGUI
