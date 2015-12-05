module Day4_2 where

import Data.Hash.MD5

secretKey :: String
secretKey = "iwrupvqb"

hashed :: Int -> String
hashed num = md5s (Str $ secretKey ++ show num)

main :: IO ()
main = putStrLn (show $ mine 0)

mine :: Int -> Int
mine inp = let try = hashed inp in
    case take 5 try of
        "000000" ->
            inp
        _ ->
            mine (inp+1)
