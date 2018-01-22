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

lista : List Integer
lista = [1, 2, 3]

lista2 : List Integer
lista2 = [1..2]

StringOrInt : Bool -> Type
StringOrInt x =
  case x of
    True  => Int
    False => String

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt x =
  case x of
    True => 94
    False => "Ninety four"

valToString : (x : Bool) -> StringOrInt x -> String
valToString x val =
  case x of
    True  => cast val
    False => val

tuple : (Integer, Integer, Integer, Integer)
tuple = (1, (2, (3, 4)))

cons : List String
cons = "kons" :: ["tig"]
