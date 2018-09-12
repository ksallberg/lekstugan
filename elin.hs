module Elin where

import Control.Monad (forM_)
import Prelude hiding((*), (+))

import qualified Prelude as P

(*) :: (Int -> IO()) -> Int -> IO ()
(*) f x = forM_ [1..x] f

(+) :: Int -> Int -> Int
(+) 2 2 = 5
(+) a b = a P.+ b

main :: IO ()
main = (\_ -> putStrLn "hej,haj") * 3
