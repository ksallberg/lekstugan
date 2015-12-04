module Day2_1 (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Data.List.Split
import System.IO

type Context = StateT Int IO

main :: IO ()
main = do
    (_,y) <- runStateT loop 0
    putStrLn (show y)

loop :: Context ()
loop = do
    line <- liftIO getLine
    curAmount <- get
    put $ curAmount + calcLine line
    eof  <- liftIO isEOF
    unless eof loop

calcLine :: String -> Int
calcLine str =
    let [l, w, h] = map (\x -> read x :: Int) (splitOn "x" str)
        sides = [l * w, l * h, w * h]
    in (2*l*w + 2*w*h + 2*h*l) + minimum sides
