-- source from https://wiki.haskell.org/Template_Haskell

{-# LANGUAGE TemplateHaskell #-}

module Main where

import PrintF (printf)
import Tuple (tmap,
              tuple)

main :: IO ()
main = putStrLn test

-- '$(' betyder splice. alltså att uttrycket
--   '$(printf "Error: %s on line %d")'
-- blir ett nytt program compile-time, med rätt typer,
-- beroende på vilken input som ges efter
test :: String
test = $(printf "Error: %s on line %d") "Bad var" 123

-- test2 :: Int
-- test2 = $(tuple 2 3) (1, 2, 3, 4, 5)

-- kan inte enkelt operera over tupel
test3 = $(tmap 3 4) (+1) (1,2,3,4)

-- kan inte enkelt gora tupel av lista?
test4 = $(tuple 4) [1,2,3,4]
