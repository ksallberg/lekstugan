module Elin where

import Control.Monad (forM_)
import Prelude hiding((*))

(*) :: (Int -> IO()) -> Int -> IO ()
(*) f x = forM_ [1..x] f

main :: IO ()
main = (\_ -> putStrLn "hej,haj") * 3
