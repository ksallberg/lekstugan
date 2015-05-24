{-
An implementation of the DD Min algorithm in Haskell. It's used for finding
the shortest local failing test input in a longer failing input. For instance,
if having two e's in a string is considered an error, and a faulty string
"hmmehmmmmmexxx" is given. This algorithm shortens the faulty input to "ee":
ddMin "hmmehmmmmmexxx" testFor2E
-}

import Data.List

data Result = Pass | Fail | Unresolved
   deriving (Show, Eq)

ddMin :: String -> (String->Bool) -> String
ddMin input testFor = ddMin' input 2 testFor

--Pseudo code for the ddmin' function
--ddmin' (c'x,n)
--ddmin' c'x                    --if |c'x| == 1
--ddmin' (c'x \ci, max(n-1,2))  --else if EXIST i in {1...n}
--                                    x test (c'x \ ci) == x
--ddmin' (c'x, min(2n, |c'x|))  --else if n < |c'x|
--ddmin' c'x                    --otherwise

--DD Min specific functions:
ddMin' :: String -> Int -> (String->Bool) -> String
ddMin' input chunks testFor
   | length input == 1      = input
   | length failedCases > 0 = ddMin' (head failedCases)
                                     (max (chunks-1) 2)
                                     testFor
   | chunks < length input  = ddMin' input
                                     (min (2*chunks)
                                     (length input))
                                     testFor
   | otherwise              = input
   where
      failedCases = [input `removeChunk` x |
                     x <- subsets, test testFor (input `removeChunk` x) == Fail]
      subsets     = createSubset chunks input

createSubset :: Int -> String -> [String]
createSubset chunks str
   | chunks > length str = error "NOOO!"
   | otherwise
   = chunk (ceiling $ intToFloat (length str) / intToFloat(chunks)) str

chunk :: Int -> String -> [String]
chunk len str | len > length str && str /= "" = [str]
              | len > length str              = []
              | otherwise                     = fst spl : chunk len (snd spl)
              where spl                       = splitAt len str

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

removeChunk :: String -> String -> String
removeChunk s find = removeChunk' s find "" False

removeChunk' :: Eq a => [a] -> [a] -> [a] -> Bool -> [a]
removeChunk' [] _ _      isRemoved = []
removeChunk' s find repl isRemoved =
    if (take (length find) s == find) && (isRemoved == False)
        then repl ++ (removeChunk' (drop (length find) s) find repl True)
        else [head s] ++ (removeChunk' (tail s) find repl isRemoved)

--Test specific functions:
test :: (String->Bool) -> String -> Result
test function str | res == True  = Fail
                  | res == False = Pass
                  where res = function str

testFor2E :: String -> Bool
testFor2E str = ((count 'e' str) >= 2)

testFor4A :: String -> Bool
testFor4A str = (count 'a' str) >= 4

-- Count the number of a char in a string
count :: Char -> String -> Int
count checkfor [] = 0
count checkfor (x:xs) | x == checkfor = 1 + count checkfor xs
count checkfor (x:xs) = count checkfor xs
