{-

{-# LANGUAGE GADTs #-}

data Empty
data NonEmpty

data List x y where
   Nil  :: List a Empty
   Cons :: a -> List a b -> List a NonEmpty
   Ext  :: a -> b -> List a b
   Ext2 :: a -> Int -> List a b

--}
{--

   det skulle inte gå att göra

   data Empty
   data NonEmpty

   data List x y = Nil x Empty
                   Cons a (List x y)
                   Ext x y
                   Ext x Int
--}

data NewEmpty
data NewNonEmpty

data List2 x y = Nil2 x NewEmpty     |
                 Cons2 x NewNonEmpty |
                 CCC x y             |
                 DDD x Int

{-

safeHead :: List x NonEmpty -> x
safeHead (Cons a b) = a

ls :: List Int NonEmpty
ls = Cons 23 Nil

ls2 :: List Int Int
ls2 = Ext 23 33

ls3 :: List Int Int
ls3 = Ext2 444 222

s2 :: List a b -> (a,b)
s2 (Ext a b) = (a,b)

s3 :: List a Int -> (a,Int)
s3 (Ext2 a b) = (a,b)

-}
