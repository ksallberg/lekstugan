-- Generalized Algebraic Datatype
-- https://www.haskell.org/haskellwiki/GADTs_for_dummies

{-# LANGUAGE GADTs #-}

-- Either      = type constructor
-- Left, Right = data constructors
data MyEither a b = MyLeft a | MyRight b

-- working with data constructors (ordinary haskell)
isLeft (MyLeft a)  = True
isLeft (MyRight b) = False

{- work with type constructors
   X is a TYPE FUNCTION names "X".

   a is a type and it returns some type as result

   we cant use X on data values but we can use it on type values

   type constructors can serve as basic "values" and type functions as a way
   to process them


-}

type X a = MyEither a a

{-
    GADTs

    "If you are wondering how all of these interesting type manipulations
    relate to GADTs, here is the answer. As you know, Haskell contains
    highly developed ways to express data-to-data functions. We also know
    that Haskell contains rich facilities to write type-to-type functions
    in the form of "type" statements and type classes. But how do "data"
    statements fit into this infrastructure?

    My answer: they just define a type-to-data constructor translation.
    Moreover, this translation may give multiple results.

    And here we finally come to GADTs! It's just a way to define data types
    using pattern matching and constants on the left side of "data"
    statements!"

    data T String = D1 Int
         T Bool   = D2
         T [a]    = D3 (a,a)

    The idea here is to allow a data constructor's return type to be
    specified directly
-}

data T a where
    D1 :: Int -> T String
    D2 :: T Bool
    D3 :: (a, a) -> T [a]

-- Amazed? After all, GADTs seem to be a really simple and obvious extension
-- to data type definition facilities.

data Term a where
    Lit  :: Int ->  Term Int
    Pair :: Term a -> Term b -> Term (a,b)

data Term2 a = Lit2 Int | Pair2 (Term2 a) (Term2 a)

{-

In a function that performs pattern matching on Term, the pattern match gives
type as well as value information.

If the argument matches Lit, it must have been built with a Lit constructor,
so type 'a' must be Int, and hence we can return 'i' (an Int) in the right
hand side. The same thing applies to the Pair constructor.
-}

eval :: Term a -> a
eval (Lit i)    = i
eval (Pair a b) = (eval a, eval b)

{-
Not possible:

eval2 :: Term2 a -> a
eval2 (Lit2 i)    = i
eval2 (Pair2 x y) = (eval2 x, eval2 y)
-}
