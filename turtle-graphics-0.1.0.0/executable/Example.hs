module Main where

import Data.List
import TurtleGraphics
import Turtle

main = runGraphical identityTurtle fancyExample2000

-- | Spiral implementation in our language
spiral :: Int -> Double -> Program
spiral size angle =
   if size > 200
      then die
      else (if size > 50 then color Blue else color Red) <*>
           forward size  <*>
           right   angle <*>
           spiral (size+2) angle

-- | Implementing the spiral in terms of lifespan and forever
spiral2 :: Int -> Double -> Program
spiral2 size angle = lifespan 100 (prog size angle)
      where prog s a = forward s <*>
                       right   a <*>
                       prog (s+2) a

testExample1 =  times 10 ((right 10 <*> forward 10 <*> penup <*> forward 10 <*> 
                        pendown) <|>
                        (left 30 <*> backward 15 <*> color Yellow <*> backward 10 <*>
                        color Blue) <|>
                        (right 90 <*> forward 10 <*> hideturtle <*> right 90 <*>
                        forward 10 <*> showturtle))
runTestExample1 = runGraphical identityTurtle testExample1

-- test lifespan and tforever
testExample2 = lifespan 30 (tforever (forward 4))
runTestExample2 = runGraphical identityTurtle testExample2

-- test die
testExample3 = (forward 50 <*> die <*> backward 50) <|>
               (right 50 <*> tforever (forward 1))
runTestExample3 = runGraphical identityTurtle testExample3

fancyExample2000 = (fancyHenchman 0    Blue   <|>
                    fancyHenchman 20   Red    <|>
                    fancyHenchman 40   Yellow <|>
                    fancyHenchman 60   Green  <|>
                    fancyHenchman 80   Blue   <|>
                    fancyHenchman 100  Red    <|>
                    fancyHenchman 120  Yellow <|>
                    fancyHenchman 140  Green  <|>
                    fancyHenchman 160  Blue   <|>
                    fancyHenchman 180  Red    <|>
                    fancyHenchman 200  Yellow <|>
                    fancyHenchman 220  Green  <|>
                    fancyHenchman 240  Blue   <|>
                    fancyHenchman 260  Red    <|>
                    fancyHenchman 280  Yellow <|>
                    fancyHenchman 300  Green  <|>
                    fancyHenchman 320  Blue   <|>
                    fancyHenchman 340  Red
                    )                         <*>
                    (times 25 (right 15 <*> forward 15)) <*> hideturtle 

fancyHenchman i col = color col <*> right i <*> forward 10
