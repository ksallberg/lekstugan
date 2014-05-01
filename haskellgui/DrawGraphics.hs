module DrawGraphics (openGraphWindow) where

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC

openGraphWindow :: (Double->Double) -> (Double,Double) -> Double -> String-> IO ()
openGraphWindow fun range waveLen title =
   do initGUI
      win <- windowNew
      windowSetTitle win title
      onDestroy win mainQuit
      can <- drawingAreaNew
      onSizeRequest can $ return (Requisition 500 300)
      onExpose can $ drawCanvas can fun range waveLen
      containerAdd win can
      widgetShowAll win
      mainGUI

drawCanvas :: DrawingArea -> (Double->Double) -> (Double,Double) -> Double -> event -> IO Bool
drawCanvas can fun range waveLen _evt =
   do dw <- widgetGetDrawWindow can
      drawWindowClear dw
      gc <- gcNew dw
      drawFunction fun dw gc [(fst range)..(snd range)] waveLen
      return True

drawFunction :: (Double->Double) -> DrawWindow -> GC -> [Double] -> Double -> IO Bool
drawFunction f dw gc []     waveLen = return True
drawFunction f dw gc (x:xs) waveLen =
   do let funVal  = round $ (f x)*waveLen
      let nextVal = round $ (f (x+1.0))*waveLen
      drawLine dw gc (round waveLen+(round x),round waveLen+funVal)
                     (round waveLen+(round $ x+1),round waveLen+nextVal)
      drawFunction f dw gc xs waveLen
