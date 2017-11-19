{-
   https://functional.works-hub.com/blog/Dependent-Types-in-Haskell-2

   1. values depending on values (function)

   2. values depending on types

   3. types depending on types

   4. types depending on values
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Tutorial where

-- Implementations:

-- 1. values depending on values (function)

a :: Integer
a = 2

b :: Bool
b = even a

-- 2. values depending on types

maxBound1 :: Int
maxBound1 = 2

maxBound2 :: Double
maxBound2 = 2

-- 3. types depending on types (type functions)

-- https://hackage.haskell.org/package/type-combinators-0.2.4.3/docs/Type-Family-List.html

data family XList a

data instance XList Char = XCons !Char !(XList Char) | XNil

data instance XList () = XListUnit !Int

type Ø    = '[]
type (:<) = '(:)

-- type family (as :: [k]) >: (a :: k) :: [k] where
--   Ø >: a = a :< Ø
--   (b :< as) >: a = b :< (as >: a)

-- type family Reverse (xs :: [a]) :: [a] where
--   Reverse Ø = Ø
--   Reverse (a :< as) = Reverse as :> a

-- 4. types depending on values

-- datatype (GADT)

data IntOrString a where
  IntCons :: Int -> IntOrString Int
  StrCons :: String -> IntOrString String

-- pattern match

wasItStringOrInt :: IntOrString a -> IntOrString b -> [Char]
wasItStringOrInt x y =
  case x of
    IntCons x' -> case y of
      IntCons y' -> show $ x' + y'
      StrCons y' -> (show x') ++ y'
    StrCons x' -> case y of
      IntCons y' -> x' ++ (show y')
      StrCons y' -> x' ++ y'

-- f :: (x :: Bool) (if x then Int else String) -> String
-- f (x, y) = case x of
--   True -> show y
--   False -> y
