module MonoList
   (
        ML (..)
      , (+>)
   )
where

import Control.Applicative
import Control.Monad
import Data.Monoid

data ML a = Init (ML a) | Place a (ML a) | End

instance (Show a) => Show (ML a) where
   show End           = "]"
   show (Init x)      = "["++(show x)
   show (Place x End) = (show x)++"]"
   show (Place x y)   = (show x)++","++(show y)

-- vad används monoid i? fold?
instance Monoid (ML a) where
   -- :: a
   mempty  = Init End
   -- :: a -> a -> a
   mappend x y = x +> y
   -- :: [a] -> a
   -- This cannot be implemented
   -- without a native list???
   mconcat = undefined

-- functor, för att kunna mappa över en struktur
instance Functor (ML) where
   -- fmap :: (a->b) -> f a -> f b
   fmap fun End         = End
   fmap fun (Init End)  = Init End
   fmap fun (Init x)    = Init (fmap fun x)
   fmap fun (Place x y) = Place (fun x) (fmap fun y)

-- applicative
instance Applicative (ML) where
   pure a           = End
   End <*> End      = End
   (Init x) <*> fa  = undefined

-- monad
instance Monad (ML) where
   return a = Init (Place a End)
   -- (>>=) :: m a -> (a->m b) -> m b
   (Init End)           >>= _ = Init End
   (Place x _)          >>= fun = fun x
   (Init (Place a End)) >>= fun = fun a

-- test return
testMonad :: ML Int
testMonad = return 23

-- test bind
testMonad2 :: ML Int
testMonad2 = do a <- return 23
                b <- return 100
                c <- return 1
                return (a+b-c)

testApplicative2 :: ML Int
testApplicative2 = (\x -> Init x ) (Place 2 End)

-- append
(+>) :: ML a -> ML a -> ML a
xs +> End         = xs -- base case
xs +> (Place x y) = (append xs x) +> y
xs +> (Init y)    = xs +> y

-- put element at last place of list
--    run through the first list until End
--    and then add the new item at last place
append :: ML a -> a -> ML a
append End x            = Place x End
append (Place x cont) y = Place x (append cont y)
append (Init cont) x    = Init (append cont x)

-- testing!!!
emptyLs :: ML Int
emptyLs = Init End

ls :: ML Int
ls = Init $ Place 23 (Place 243 End)
