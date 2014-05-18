{-# LANGUAGE GADTs #-}

data Empty
data NonEmpty

data List x y where
   Nil  :: List a Empty
   Cons :: a -> List a b -> List a NonEmpty
   Ext  :: a -> b -> List a b
   Ext2 :: a -> Int -> List a b

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
