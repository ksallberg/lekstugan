{-# LANGUAGE OverloadedStrings #-}

import Data.Tuple
import Data.Char
import qualified Data.Text as DT
import Control.Monad
import System.IO
import qualified Data.List as DL

isPalindrome :: String -> Bool
isPalindrome x = reverse x == x

anagrams :: String -> [String]
anagrams = DL.permutations

-- getNumDeletions :: String -> Int
-- getNumDeletions str = getNumDeletions' 0 str

-- getNumDeletions' :: Int -> String -> Int
-- getNumDeletions' count str =
--   case (any isPalindrome deletedAna) of
--     True  -> count
--     False -> getNumDeletions' (count+1) str
--   where deletedAna = map (drop count) (anagrams str)

-- firstPalin :: Int -> String -> String
-- firstPalin count str =
--   case (any isPalindrome deletedAna) of
--     True -> head (DL.nub [ana | ana <- deletedAna, isPalindrome ana])
--     False -> firstPalin (count+1) str
--   where deletedAna = map (drop count) (anagrams str)

getNumPalins :: Int -> String -> Int
getNumPalins count str =
  let anas = anagrams str
  in length [ana | ana <- DL.nub anas, isPalindrome ana]

-- getResult :: String -> (Int, Int)
-- getResult str = (del, getNumAnagrams del palin)
--   where del   = getNumDeletions str
--         palin = firstPalin 0 str

go s = map swap $ map (\x->([head x], length x)) . DL.group . DL.sort $ s

-- findPalin :: String -> String
-- findPalin str = case (length str) `mod` 2 == 0 of
--                    True  ->
--                      -- have odd frequency?
--                      let odds = oddFreq (go str)
--                    False ->
--    where fre = go str

type Freq = (Int, String)

--allEven :: [

--removesLeast

removeOne :: String -> (String, Int)
removeOne str = let x = foldl (\res (i, toRemove) ->
                                 replaceO toRemove res
                              ) str frList
                in (x, sum [i | (i, _) <- frList])
  where frList = case DL.sort $ (oddFreq (go str)) of
          [] -> []
          [x] -> []
          x  -> init x

replaceO :: String -> String -> String
replaceO x ls = [l | l <- ls, [l] /= x]

oddFreq :: [Freq] -> [Freq]
oddFreq str = [s | s@(i,c) <- str, odd i]
  where odd x = x `mod` 2 /= 0

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

fx [] = []
fx ('\n':' ':a) = fx ('\n' : a)
fx (a:b) = a : fx b

result2 :: String -> (Int, Int)
result2 str = let (s, le) = removeOne str
              in (le, getNumPalins le s)

main :: IO ()
main = do
  line <- getLine
  let (x, y) = result2 line
  putStrLn $ (show x) ++ "," ++ (show y)
  hFlush stdout
  eof <- isEOF
  unless eof main
