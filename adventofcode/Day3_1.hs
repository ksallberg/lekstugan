module Day3_1 (main) where

import Data.List

main :: IO ()
main = do
    input <- getLine
    let houses = length $ nub $ step input [(0,0)]
    putStrLn (show houses)

type House = (Int, Int)

step :: String -> [House] -> [House]
step [] hs         = hs
step (dir:dirs) hs = step dirs (nextHouse dir (head hs) : hs)

nextHouse :: Char -> House -> House
nextHouse '^' (x, y) = (x, y-1)
nextHouse 'v' (x, y) = (x, y+1)
nextHouse '<' (x, y) = (x-1, y)
nextHouse '>' (x, y) = (x+1, y)
