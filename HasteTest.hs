import Haste
import Haste.Graphics.Canvas

-- Rendering a picture:
--
--   1. Start with basic shapes, such as `line`, `circle`, etc.
--   2. Combine shapes using do-notation
--   3. Make the shape visible by using either of
--
--         stroke :: Shape () -> Picture ()  -- Draw the contours
--         fill   :: Shape () -> Picture ()  -- Fill with solid color
--
--   4. Combine pictures using do-notation
--   5. Show the picture on a canvas using
--
--         render :: Canvas -> Picture a -> IO a

snowMan :: Double -> Shape ()
snowMan x = do
    circle (x,100) 20
    circle (x,65) 15
    circle (x,40) 10

twoSnowMenInABox :: Picture ()
twoSnowMenInABox = do
    fill   $ snowMan 100
    stroke $ snowMan 200
    stroke $ rect (50,10) (250,150)

main :: IO ()
main = do
    Just can <- getCanvasById "canvas"
    render can twoSnowMenInABox
