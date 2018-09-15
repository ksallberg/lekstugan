module Qsort where

ls :: [Int]
ls = [93, 98, 59, 29, 100, 9, 23, 45, 7, 12, 1, 99,
      -2, 0, 15, 4, 11, 9, 32, -10, -11, 95, 92]

qsort :: [Int] -> [Int]
qsort [] = []
qsort (piv:all) = qsort smal ++ [piv] ++ qsort larg
  where larg = larger piv all
        smal = smaller piv all

larger :: Int -> [Int] -> [Int]
larger pivot ls = [l | l <- ls, l > pivot]

smaller :: Int -> [Int] -> [Int]
smaller pivot ls = [l | l <- ls, l < pivot]
