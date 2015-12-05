module Day2_2 (main) where

import Control.Monad
import Control.Monad.State.Lazy
import Data.List.Split
import Data.List
import System.IO

type Context = State Int

main :: IO ()
main = do
    c <- getContents
    let (_, y) = runState (loop (lines c)) 0
    putStrLn (show y)

loop :: [String] -> Context ()
loop [] = return ()
loop (l:ls) = do
    curAmount <- get
    put $ curAmount + calcDatRibbon l
    loop ls

calcDatRibbon :: String -> Int
calcDatRibbon str =
    let x@[a, b, c]   = sort $ map (\x -> read x :: Int) (splitOn "x" str)
        perimeters    = [a+b+a+b, a+c+a+c, c+b+c+b]
        distanceRound = a + a + b + b
    in (minimum $ distanceRound : perimeters) + a*b*c
