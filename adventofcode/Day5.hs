module Day5 (main) where

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
    case isNiceLinePt2 line of
        True  -> put $ curAmount + 1
        False -> return ()
    eof <- liftIO isEOF
    unless eof loop

isNiceLine :: String -> Bool
isNiceLine str = threeVowels str && twice str && notContains str

isNiceLinePt2 :: String -> Bool
isNiceLinePt2 str = repeats str && has2Pairs str

threeVowels :: String -> Bool
threeVowels str = length (filter ((flip elem) "aeiou") str) >= 3

twice :: String -> Bool
twice []  = False
twice [_] = False
twice (a:b:xs) = a == b || twice (b:xs)

notContains :: String -> Bool
notContains str = not $ any ((flip isInfixOf) str) ["ab", "cd", "pq", "xy"]

repeats :: String -> Bool
repeats [] = False
repeats [_, _] = False
repeats (a:b:c:xs) = a == c || repeats (b:c:xs)

has2Pairs :: String -> Bool
has2Pairs str = step2Pairs pairs str
    where pairs = allPairs str

step2Pairs :: [String] -> String -> Bool
step2Pairs [] _ = False
step2Pairs (p:ps) str =
    case length (splitOn p str) >= 3 of
        True  -> True
        False -> step2Pairs ps str

allPairs :: String -> [String]
allPairs [] = []
allPairs [_] = []
allPairs (x:y:z) = [x,y] : allPairs (y:z)
