module Day3_2 (main) where

import Data.List

main :: IO ()
main = do
    input <- getLine
    let (santa1Dirs, santa2Dirs) = splitDirections input
        housesSanta1 = step santa1Dirs [(0,0)]
        housesSanta2 = step santa2Dirs [(0,0)]
        combinedHouses = length $ nub $ housesSanta1 ++ housesSanta2
    putStrLn (show combinedHouses)

type House = (Int, Int)

splitDirections :: String -> (String, String)
splitDirections dirs =
    splitDirections' dirs 0 ([], [])
    where splitDirections' [] _index ret = ret
          splitDirections' (d:ds) index (s1, s2) =
              case mod index 2 of
                  0 -> splitDirections' ds (index+1) (s1++[d], s2)
                  1 -> splitDirections' ds (index+1) (s1, s2++[d])

step :: String -> [House] -> [House]
step [] hs         = hs
step (dir:dirs) hs = step dirs (nextHouse dir (head hs) : hs)

nextHouse :: Char -> House -> House
nextHouse '^' (x, y) = (x, y-1)
nextHouse 'v' (x, y) = (x, y+1)
nextHouse '<' (x, y) = (x-1, y)
nextHouse '>' (x, y) = (x+1, y)
