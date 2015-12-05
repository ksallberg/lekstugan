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
    case isNiceLine line of
        True  -> put $ curAmount + 1
        False -> return ()
    eof <- liftIO isEOF
    unless eof loop

isNiceLine :: String -> Bool
isNiceLine str = threeVowels str && twice str && notContains str

threeVowels :: String -> Bool
threeVowels str = length (filter ((flip elem) "aeiou") str) >= 3

twice :: String -> Bool
twice []  = False
twice [_] = False
twice (a:b:xs) = a == b || twice (b:xs)

notContains :: String -> Bool
notContains str = not $ any ((flip isInfixOf) str) ["ab", "cd", "pq", "xy"]
