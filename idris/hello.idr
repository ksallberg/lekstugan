module Main

import Data.Vect

main : IO ()
main = putStrLn "Hello, Idris world!"

-- test : Vect 3 Nat
test : Vect 1 Char
test = ['b']

test43 : Vect 1 Char
test43 = ['a']

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

-- Implement code where input (QueryType) is "linked" to output (QueryResult)
-- and that the code can not give the wrong result back.
data QueryType = Financials | Company | Batch

data QueryResult : QueryType -> Type where
  FinancialsResult : QueryResult Financials
  CompanyResult : (companyName : String) -> QueryResult Company
  BatchResult : (amount : Nat) -> QueryResult Batch

doQuery : (x : QueryType) -> QueryResult x
doQuery Financials = FinancialsResult
doQuery Company = CompanyResult "aapl"
doQuery Batch = BatchResult 42

-- doQuery Batch = FinancialsResult --BatchResult 42

{-
- + Errors (1)
 `-- hello.idr line 63 col 16:
     When checking right hand side of doQuery with expected type
             QueryResult Batch

     Type mismatch between
             QueryResult Financials (Type of FinancialsResult)
     and
             QueryResult Batch (Expected type)

     Specifically:
             Type mismatch between
                     Financials
             and
                     Batch
-}
