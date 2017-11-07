{-# LANGUAGE TemplateHaskell #-}
module Tuple where

-- Import some Template Haskell syntax
import Language.Haskell.TH
import Control.Monad

tmap :: Int -> Int -> ExpQ
tmap i n = do
    f <- newName "f"
    as <- replicateM n (newName "a")
    lamE [varP f, tupP (map varP as)] $
        tupE [  if i == i'
                    then [| $(varE f) $a |]
                    else a
               | (a,i') <- map varE as `zip` [1..] ]

tuple :: Int -> ExpQ
tuple n = do
    ns <- replicateM n (newName "x")
    lamE [foldr (\x y -> conP '(:) [varP x,y]) wildP ns] (tupE $ map varE ns)
