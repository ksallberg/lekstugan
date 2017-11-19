-- source from https://wiki.haskell.org/Template_Haskell

{-# LANGUAGE TemplateHaskell #-}

module Main where

import PrintF (printf)
import Tuple (tmap,
              tuple)

{-
template$ ghci -XTemplateHaskell
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
Prelude> :m + Language.Haskell.TH
Prelude Language.Haskell.TH> runQ [| \x -> 1 |]
LamE [VarP x_0] (LitE (IntegerL 1))
-}

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

{-|
 I $(tuple 4) skapar man vid compile time en funktion
 som opererar over en tupel med 4 element.

 Sjalva funktionen genererar man med kod i Tuple.hs
 (metaprogrammering), man kan pa sa satt skapa generisk
 kod istallet for att behova hårdkoda konverteringen från
 lista till tupel direkt i haskellkod.

 Jämför med test4_no_meta under som *måste* vara hårdkodad.
 Samma sak att läsa från en godtycklig plats i en tupel.
-}
test4 :: (Int, Int, Int, Int)
test4 = $(tuple (length [1,2,3,4])) [1,2,3,4]

test4_no_meta :: (Int, Int, Int, Int)
test4_no_meta = test4_no_meta' [1,2,3,4]

test4_no_meta' :: [Int] -> (Int, Int, Int, Int)
test4_no_meta' [a, b, c, d] = (a, b, c, d)
