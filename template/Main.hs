-- source from https://wiki.haskell.org/Template_Haskell

{-# LANGUAGE TemplateHaskell #-}

module Main where

import PrintF (printf)

main :: IO ()
main = putStrLn test

-- '$(' betyder splice. alltså att uttrycket
--   '$(printf "Error: %s on line %d")'
-- blir ett nytt program compile-time, med rätt typer,
-- beroende på vilken input som ges efter
test :: String
test = $(printf "Error: %s on line %d") "Bad var" 123
