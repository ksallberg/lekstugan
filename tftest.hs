{-# LANGUAGE TypeFamilies, GADTs #-}

data Zero
data Suc n
data Vec a n where
    Nil  :: Vec a Zero
    Cons :: a -> Vec a n -> Vec a (Suc n)

instance Show (Vec a n) where
    show (Nil)= "hej"

type family XAdd m n :: *
type instance XAdd Zero n = n
type instance XAdd (Suc m) n = Suc (XAdd m n)

(+++) :: Vec a m -> Vec a n -> Vec a (XAdd m n)
Nil        +++  ys  =  ys
Cons x xs  +++  ys  =  Cons x (xs +++ ys)

test2 = Nil +++ Nil

test :: XAdd Zero Int -> XAdd Zero Int
test x = x

tes :: XAdd (Suc Int) Int -> XAdd (Suc Int) Int
tes u = u

data Numb = Base | More Numb
    deriving Show

three :: Numb
three = More $ More $ More Base

evalNumb :: Numb -> Int
evalNumb Base = 0
evalNumb (More x) = 1 + evalNumb x

data Fin n where
    FZero :: Fin (Suc n)
    FCont :: Fin n -> Fin (Suc n)

two :: Fin (Suc (Suc (Suc a)))
two = FCont $ FCont FZero

evalFin :: Fin n -> Int
evalFin FZero     = 0
evalFin (FCont x) = 1 + evalFin x

(!) :: Vec a n -> Fin n -> a
(Cons a _)       ! FZero         = a
(Cons _ nextVal) ! FCont nextPos = nextVal ! nextPos
