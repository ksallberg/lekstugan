{-

-When I run cabal haddock I get several missing link destination, 
 typically this means you have a data type that's used by your 
 exported functions but noteven the name of the data type is 
 exported (Why is this bad?).

 >>> This is now fixed, we're exporting all Data types used by
     the turtle language. (See file Turtle.hs)

> However, things will look the same when finished drawing.
- Not quite true (consider coloring)
 
 >>> Ok. That is true! We changed that statement. (report.txt)

- When I run forward 10 <*> (forward 10 <|> forward 10)
I get only one turtle?! (tested with the textual interface)
I think there is some fundamental flaw in the design of your parallel turtles.
 
 >>> The problem was that <*> was concatenating several parallel
     turtles combining them to become just one.

- You write that run gives a turtle back, but you don't say how it is 
  different from the input turtle. Has it moved one step 
  or as many as it can many?

 >>> Fixed this in the comments of the generalized run function (Turtle.hs)

Suggestions:
- A default turtle would be helpful 

 >>> added that, it's called identityTurtle in Turtle.hs

- You should use record syntax for the run function.

 >>> fixed that in Turtle.hs

-}
-- | This module provides a domain specific language for
--   moving a turtle on the screen. The idea is that a program
--   is a list of instructions (for instance GoLeft 300). Programs
--   can then be sequenced and combined.
module Turtle (
  -- | The turtle type(s)
   Operation(..)
 , Actions
 , Program
 , Turtle(..)
 , TColor(..)
 , TPoint

  -- | Primitive operations
 , forward
 , backward
 , left
 , color
 , penup
 , pendown
 , die
 , tforever
 , times
 , showturtle
 , hideturtle

  -- | Derived operations
 , right
 , lifespan
 
  -- | Constructor functions
 , identityTurtle

  -- | Combinator functions
 , (<*>)
 , (<|>)

  -- | Run function
 , run
 , runTextual
  -- ...
  ) where

-- | A simple cartesian plane point
type TPoint = (Double,Double)

-- | Modeling Colors
data TColor = Yellow | Blue | Red | Green
        deriving (Show, Eq, Enum)

-- | To simplify for the user and to make parallel composition easier to use,
--   we decided to split our previous Program type (from hand in #1)
--   into two types. Actions is what we previously defined as a Program, a 
--   list of Operations. Now a Program is a list of a list of Operations, 
--   this way the user doesn't need to give an explicit list when writing
--   a program
type Actions = [Operation]
type Program = [Actions]

-- | Change from hand in #1: We have removed a couple of
--   operations, instead using for instance Forward (-1)
--   as our old Backward 1
data Operation =
   GoLeft   Double |
   Forward  Double |
   PenUp           |
   PenDown         |
   SetColor TColor | 
   ShowTurtle      |
   HideTurtle      |
   End
   deriving (Show,Eq) --Eq so that we can test properties

-- | Data type for keepig the state of a turtle 
data Turtle = Turtle
   {   position  :: TPoint
     , degree    :: Double
     , isPenDown :: Bool
     , turColor  :: TColor
     , isVisible :: Bool
     , isDead    :: Bool
   }
   deriving (Show)

-- | Constructors
--   Create a standard default turtle
identityTurtle :: Turtle
identityTurtle = Turtle (250,250) 90.0 True Red True False

-- | Combinators
--   for all programs in the list of programs to run,
--   add the given program at the end
--
--   If the user combines programs which are both
--   parallel, then the carthesian product will be applied.
--   If not, a simple list concatenation is performed
(<*>) :: Program -> Program -> Program
e <*> a = [new++new2 | new <- e, new2 <- a]

-- | Parallel composition
--   Adds a program to the list of parallel programs
(<|>) :: Program -> Program -> Program
p <|> q = p ++ q

-- | Abstract run function that takes a turtle and an operation and
--   gives a new turtle back, that new turtle has moved just one step.
--   compared to the given (old) turtle.
--
--   We're now using record syntax..
--
--   This run function is used by other concrete run functions.
--   This is where most of the work happens because we have a deep
--   embedding. 
run :: Turtle -> Operation -> Turtle
-- Add a left/right operation to the new turtle
run tur (GoLeft a) = tur {degree=(degree tur)+a}
-- Add a forward/backward operation to the new turtle
run tur (Forward a) = tur {position=calcPos (position tur) (degree tur) a}
-- Add a PenUp operation to the new turtle
run tur PenUp = tur {isPenDown=False}
-- Add a PenDown operation to the new turtle
run tur PenDown = tur {isPenDown=True}
-- Add a color operation to the new turtle
run tur (SetColor c) = tur {turColor=c}
-- Add a show turtle command to the new turtle
run tur ShowTurtle = tur {isVisible=True}
-- Add a hide turtle command to the new turtle
run tur HideTurtle = tur {isVisible=False}
-- Add the final End command to a turtle, making it stop a run
run tur End = tur {isDead=True}

-- | Calculate the new position given the old point, degree and distance.
calcPos :: TPoint -> Double -> Double -> TPoint
calcPos (oldX,oldY) degree dist = ( (oldX + (cos (toRad degree)) * dist),
                                    (oldY + (sin (toRad degree)) * dist) )
        where toRad x = 2* pi * x / 360

-- | Run function for printing a program to the
--   terminal, can print both finite and infinite 
--   programs.
runTextual :: Turtle -> Program -> IO()
runTextual t p = runTextual' (zip (repeat t) p)

-- | Using lazyness to grab one step in each parallel
--   program and prints it to the terminal
runTextual' :: [(Turtle,Actions)] -> IO ()
runTextual' ls = do let newPairs = [(run x (head y),tail y) 
                                   | (x,y)<-ls,y /= [] && not (isDead x)
                                   ]
                    case newPairs of
                       [] -> return ()
                       _  -> do putStrLn "-------------run in par:"
                                mapM_ (putStrLn . show . fst) newPairs
                                runTextual' newPairs

-- | Primitive operations
--   Forward replicates itself to make our graphical
--   animation look smoother
forward :: Int -> Program
forward steps = [replicate steps (Forward 1)]

backward :: Int -> Program
backward steps = [replicate steps (Forward (-1))]

left :: Double -> Program
left degrees = [[GoLeft degrees]]

color :: TColor -> Program
color c = [[SetColor c]]

penup :: Program
penup = [[PenUp]]

pendown :: Program
pendown = [[PenDown]]

showturtle :: Program
showturtle = [[ShowTurtle]]

hideturtle :: Program
hideturtle = [[HideTurtle]]

die :: Program
die = [[End]]

-- | Given a program, repeat it in infinity,
--   users will then exploit haskell's lazy
--   properties to get information from it.
--   Works on parallel composition as well.
tforever :: Program -> Program
tforever prog = map (concat . repeat) prog 

-- | times takes an existing program,
--   and a number of times to repeat the
--   program. Then it returns a new program
--   repeated that number of times.
--   Works on parllel composition as well.
times :: Int -> Program -> Program
times t p = map (concat . replicate t) p

-- | Derived operation
right :: Double -> Program
right degrees = left (-degrees)

-- | Potentially shorten a program to be limited by
--   a given amount of steps.
--   Works on parallel composition as well
lifespan :: Int -> Program -> Program
lifespan times p = map (take times) p
