{-
   Own implementation of the Maybe monad
   I just with Strings right now
-}

import Control.Monad
import Data.Maybe

-- data type to wrap the content
data MyMaybe a = MyNothing | MyJust a
   deriving Show

-- monad declaration

instance Monad MyMaybe where
   return x         = MyJust x
   MyNothing  >>= f = MyNothing
   (MyJust x) >>= f = f x
   fail _           = MyNothing

-- Test the bind >>== of Nothing
-- using the monad with the do notation
testBind :: MyMaybe String
testBind = do MyNothing
              MyNothing
              MyNothing
              MyNothing

-- Test of binding (>>=) x to
-- "ha" and then giving it back
-- at the last line while first
-- creating "ho"
testBind2 :: MyMaybe String
testBind2 = do (MyJust "yo!")
               x <- return (MyJust "bind")
               (MyJust "do not bind")
               x

-- get the just part of mymaybe
getMyJust :: MyMaybe String
getMyJust = return "hi!"

-- get the nothing part of mymaybe
getMyNothing :: MyMaybe String
getMyNothing = MyNothing

-- get the just part of maybe
getJust :: Maybe String
getJust = return "hello"

-- get the nothing part of maybe
getNothing :: Maybe String
getNothing = Nothing

-- like fromJust
getBack :: MyMaybe String -> String
getBack MyNothing  = "nothing"
getBack (MyJust x) = x

-- run some tests
main :: IO ()
main = do putStrLn "get Just from my monad:"
          putStrLn $ getBack getMyJust
          putStrLn "get Just from the Maybe monad"
          putStrLn $ fromJust getJust
          putStrLn "get Nothing from my monad"
          putStrLn $ show getMyNothing
          putStrLn "testing the bind1 function"
          putStrLn $ show testBind
          putStrLn "testing the bind2 function"
          putStrLn $ getBack testBind2
