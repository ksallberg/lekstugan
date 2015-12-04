module Day2_2 (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Data.List.Split
import Data.List
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
    put $ curAmount + calcDatRibbon line
    eof <- liftIO isEOF
    unless eof loop

calcDatRibbon :: String -> Int
calcDatRibbon str =
    let x@[a, b, c]   = sort $ map (\x -> read x :: Int) (splitOn "x" str)
        perimeters    = [a+b+a+b, a+c+a+c, c+b+c+b]
        distanceRound = a + a + b + b
    in (minimum $ distanceRound : perimeters) + a*b*c
