{-# LANGUAGE TemplateHaskell #-}

module Main where

import PrintF (printf)

test :: String
test = $(printf "Error: %s on line %d" "Bad var" 123
