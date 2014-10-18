{-# LANGUAGE UnicodeSyntax #-}

{-
   For practicing failure detector relations
      (reductions and equivalences)

   run by: runhaskell FDRelations.hs
-}

module FDRelations (main) where

import Data.List
import Control.Monad
import Control.Applicative
import System.Random (randomRIO)

data FD =
    P    -- perfect completeness, perfect accuracy
  | S    -- perfect completeness, weak accuracy
  | Q    -- weak completeness, perfect accuracy
  | W    -- weak completeness, weak accuracy
  | E FD -- wrapper for eventually
    deriving (Eq)

-- provide special format for the eventually wrapper
instance Show FD where
   show P     = "P"
   show Q     = "Q"
   show W     = "W"
   show S     = "S"
   show (E x) = "⋄" ++ show x

-- run function
main :: IO ()
main = do p <- pick pairLs
          newRound p (≼) "≼"
          newRound p (≃) "≃"
          main

-- pair of FDs -> eval function -> message
newRound :: (FD,FD) -> (FD->FD->Bool) -> String -> IO ()
newRound (x,y) f oper =
   do putStrLn ("Is " ++ show x ++ oper
                ++ show y ++ "? yes/no/help?")
      answer <- getLine
      case answer of
         "help" -> printHelp
         _      -> putStrLn $ evalAnswer (f x y) answer

-- print through all rules True
printHelp :: IO ()
printHelp = do
   let allRed = [(x,y)|(x,y)<-pairLs,x ≼ y]
       allEq  = [(x,y)|(x,y)<-pairLs,x ≃ y]
   forM_ allRed (\(x,y)->putStrLn (redMessage x y))
   forM_ allEq  (\(x,y)->putStrLn (eqMessage x y))

-- get a suiting answer
evalAnswer :: Bool -> String -> String
evalAnswer True  "no"  = "Wrong answer!"
evalAnswer True  "yes" = "Good!"
evalAnswer False "yes" = "Wrong answer!"
evalAnswer False "no"  = "Good!"
evalAnswer _     _     = "Unsupported choice"

-- rules for how failure detectors are reducible
(≼) :: FD -> FD -> Bool
E S ≼ S   = True
E S ≼ E P = True
E S ≼ P   = S ≼ P || E P ≼ P -- True through transitivity
E P ≼ P   = True
S   ≼ P   = True
E W ≼ W   = True
E W ≼ E Q = True
E W ≼ Q   = W ≼ Q || E Q ≼ Q -- True through transitivity
E Q ≼ Q   = True
W   ≼ Q   = True
x   ≼ y   | x == y    = True
          | otherwise = False

-- rules for equivalency
(≃) :: FD -> FD -> Bool
P   ≃ Q   = True
Q   ≃ P   = True
S   ≃ W   = True
W   ≃ S   = True
E x ≃ E y = x ≃ y
x   ≃ y   | x == y    = True
          | otherwise = False

-- wrap a failuredetector to
(⋄) :: FD -> FD
(⋄) (E x) = error "no eventually in eventually!"
(⋄) x     = E x

-- everything to permutate and pair together
toPermutate :: [FD]
toPermutate = [P,Q,W,S,(⋄)P,(⋄)Q,(⋄)W,(⋄)S]

-- get all pairs (cartesian product)
pairLs :: [(FD,FD)]
pairLs = (,) <$> toPermutate <*> toPermutate

-- create a message for a reducible pair
redMessage :: FD-> FD -> String
redMessage x y = show x ++ " is reducible to " ++ show y

-- create a message for an equivalent pair
eqMessage :: FD -> FD -> String
eqMessage x y = show x ++ " and " ++ show y ++ " are equivalent"

-- pick random from list
pick :: [a] -> IO a
pick xs = do r <- randomRIO (0,length xs-1)
             return $ xs !! r
