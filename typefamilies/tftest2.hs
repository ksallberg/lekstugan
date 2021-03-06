{-# LANGUAGE TypeFamilies, GADTs #-}

data Zero
data Suc n
data Vec a n where
    Nil  :: Vec a Zero
    Cons :: a -> Vec a n -> Vec a (Suc n)

instance (Show a) =>Show (Vec a n) where
    show Nil           = "| nil"
    show (Cons x next) = show x ++ ", " ++ show next

type family XAdd m n :: *
type instance XAdd Zero n    = n
type instance XAdd (Suc m) n = Suc (XAdd m n)

(+++) :: Vec a m -> Vec a n -> Vec a (XAdd m n)
--(+++) :: Vec a n -> Vec a n -> Vec a (Vec a n)
Nil        +++  ys  =  ys
Cons x xs  +++  ys  =  Cons x (xs +++ ys)

{-
λ> :t (Nil +++ Nil)
(Nil +++ Nil) :: Vec a Zero
λ> :t Cons 10 Nil
Cons 10 Nil :: Num a => Vec a (Suc Zero)
λ> :t (Cons 10 (Cons 20 Nil))
(Cons 10 (Cons 20 Nil)) :: Num a => Vec a (Suc (Suc Zero))
-}
