module Main

import Data.Vect

main : IO ()
main = putStrLn "Hello, Idris world!"

-- test : Vect 3 Nat
test : Vect 1 Char
test = ['b']

test2 : Vect 2 Char
test2 = ['a', 'b']

test3 : Vect 3 Char
test3 = test ++ test2
