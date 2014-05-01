-- | A graphical run function for the turtle DSL
module TurtleGraphics (
    runGraphical
  ) where

import Control.Concurrent
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Turtle

-- | Creates a GTK window, creates two state "variables" (drawState,history)
--   drawState keeps track of what to draw in each round and 
--   history keeps track of everthing that has previously been drawed to
--   screen.
--
--   We're using the timeoutAdd function to define a timer that calls
--   exposeEvent on the window. This is why we need the two state "variables",
--   since we are calling exposeEvent on small chunks of the total data to draw
--   it's not possible to call a function that recurses to itself and increases
--   some internal state that would cause GTK to freeze
runGraphical :: Turtle -> Program -> IO ()
runGraphical turt progs = 
   do let pairs = zip3 (repeat turt) progs [0..]
      drawState <- newMVar pairs -- keep track of all pairs
      history <- newMVar (replicate (length pairs) [turt])
      initGUI
      window <- windowNew
      timeoutAdd (widgetQueueDraw window >> return True) 30
      set window [windowTitle := "Hello Cairo",
                  windowDefaultWidth := 500, windowDefaultHeight := 500,
                  containerBorderWidth := 30 ]

      window `on` exposeEvent $
        do drawWin <- eventWindow
           liftIO $ do
           currentState <- readMVar drawState
           currentHistory <- readMVar history
           let newPairs = [ (run tur (head prog), tail prog,i)
                          | (tur,prog,i) <- currentState
                          , prog /= [] && not (isDead tur)
                          ]
               -- puttingthe latest turtle in the last place of the history
               addToHistory = [ (i,tur)  | (tur, prog, i) <- newPairs ]
               -- :: [[Turtle]] -> [(Int,[Turtle])] -> [[Turtle]]
               newHistory = [ ls ++ safeGet i addToHistory--addToHistory 
                            | (i,ls) <- (zip [0..] currentHistory)
                            ]
           renderWithDrawable drawWin (drawTurt newHistory)
           modifyMVar_ drawState (\x -> return newPairs)
           modifyMVar_ history (\x -> return newHistory)
           return True
      onDestroy window mainQuit
      widgetShowAll window
      mainGUI

-- | In "newHistory" above, if the list of new calculactions to
--   add to the history is empty, we just give an empty list back
--   so that we can get the current history back
safeGet :: Int -> [(Int, Turtle)] -> [Turtle]
safeGet i ls = [t | (id,t) <- ls, i == id ]

setRGB :: TColor -> Render ()
setRGB Red     = setSourceRGB 1 0 0
setRGB Blue    = setSourceRGB 0 0 1
setRGB Yellow  = setSourceRGB 1 1 0
setRGB Green   = setSourceRGB 0 1 0

-- | Given a list with all the previous states of the turtles,
--   drawTurt draws a line between them with the given 
--   turtle parameters.
drawTurt :: [[Turtle]] -> Render ()
drawTurt t = do mapM_ drawTurt' t
                return ()

drawTurt' :: [Turtle] -> Render ()
drawTurt' []  = return ()
drawTurt' [x] = 
    case isVisible x of
        False -> return ()
        True  -> drawTurtle (fst (position x)) (snd (position x)) (degree x)
drawTurt' (x:y:xs) = do
                    setRGB (turColor x)
                    case (isPenDown x) of
                       False -> return ()
                       True  ->  do 
                          moveTo (fst(position x)) (snd(position x))
                          lineTo (fst(position y)) (snd(position y))
                          stroke
                    drawTurt' (y:xs)

-- | Draw a turtle given x, y and rotation
--   Saves the current draw state in GTk,
--   performs translation and rotation and then
--   goes back to the same draw state as before
drawTurtle :: Double -> Double -> Double -> Render ()
drawTurtle x y rot =
   do save
      setSourceRGB 0.38 0.62 0.26
      translate x y
      arc 0 0 10 (360 * pi/180) (359*pi/180)
      rotate (rot*pi/180)
      moveTo 0 0
      lineTo (-12) (-12)
      moveTo 0 0
      lineTo 12 12
      moveTo 0 0
      lineTo 12 (-12)
      moveTo 0 0
      lineTo (-12) 12
      stroke
      arc 0 0 10 (360*pi/180) (359*pi/180)
      arc 15 0 5 (360*pi/180) (359*pi/180)
      restore
      setSourceRGBA 0 0.4 0.2 1
      fill
      stroke
