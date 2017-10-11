-- source from https://wiki.haskell.org/Template_Haskell

{-# LANGUAGE TemplateHaskell #-}

module Main where

import PrintF (printf)

main :: IO ()
main = putStrLn test

test :: String
test = $(printf "Error: %s on line %d") "Bad var" 123
