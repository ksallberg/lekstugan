-- INTRODUKTION TILL FUNKTIONELL PROGRAMMERING
-- LABORATION 3 - SUDOKU SOLVER
-- Joel Ödlund, Kristian Sällberg

-- Notes:  All QuickCheck properties, data types and 
-- instances at the end. 

-- All error checking in functions omitted in final code
-- for performance.

module Sudoku where

import Data.Char
import Data.List
import Data.Maybe
import Test.QuickCheck
-------------------------------------------------------------------------
type Block = [(Maybe Int)]
type Pos = (Int,Int)

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
   deriving (Show, Eq)

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- isSudoku sud checks if sud is really a valid 
-- representation of a sudoku puzzle
isSudoku:: Sudoku -> Bool
isSudoku sud =
   (length (rows sud) == 9 ) &&
   (and [length row == 9| row <- rows sud]) && 
   (and [cell `elem` [1..9] | cell <- catMaybes (concat (rows sud))])

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved = (notElem Nothing).concat.rows

-------------------------------------------------------------------------

-- printSudoku prints a representation of a sudoku on the screen
printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . unlines . format
   where format sud         = map (map cell2char) (rows sud)
         cell2char Nothing  = '.'
         cell2char (Just n) =  intToDigit n

-- readSudoku reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath
   = do
   file <- readFile filePath
   let strLines      = lines file
       parseChar '.' = Nothing
       parseChar x   = (Just (digitToInt x))
       sud           = Sudoku (map (map parseChar) strLines)
   if (isSudoku sud)
      then
         return (sud :: Sudoku)
      else
         error "readSudoku: Error! It's not a valid sudoku!"

----------------------------
--  Part 2 of Lab
-----------------------------

-- isOkayBlock looks for multiple occurences of 
-- a (Maybe Int) in a given list
isOkayBlock :: [(Maybe Int)] -> Bool
isOkayBlock ls = catMaybes ls == nub (catMaybes ls)

-- blocks returns all rows, colums and regions as a list
-- of blocks 
blocks :: Sudoku -> [Block]
blocks sud = rows sud ++ transpose (rows sud) ++ regions (rows sud)

-- regions is a helper function for blocks. Takes a list of rows 
-- and returs a list of corresponding regions in this order:
-- [ 1 ] [ 2 ] [ 3 ]
-- [ 4 ] [ 5 ] [ 6 ]
-- [ 7 ] [ 8 ] [ 9 ]
regions :: [Block] -> [Block]
regions [] = []
regions ls
   | (head ls) /= []
      = concat (map (take 3) (take 3 ls)) 
      : (regions ((map (drop 3) (take 3 ls)) ++ (drop 3 ls)))
   | otherwise = regions (drop 3 ls) 

-- isOkay checks if a Sudoku is free from duplicate digits
-- in rows, columns and regions.
isOkay :: Sudoku -> Bool
isOkay = and.(map isOkayBlock). blocks 

-- blank returns the position of a blank cell in a given sudoku
-- creates a list of all cells, and uses elemIndex to get an index value.
-- A Pos tuple is generated from this index value using integer division
-- and modulus.
blank :: Sudoku -> Pos
blank sud = (fromJust index `div` 9, fromJust index `mod` 9)
   where index = elemIndex Nothing (concat (rows sud))
 
-- !!= Replaces an element in a list at a given index starting at 0
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (i,a) =  take i xs ++ [a] ++ drop (i+1) xs 

-- update returns a sudoku, updated with a new value at a given position

update :: Sudoku -> Pos -> (Maybe Int) -> Sudoku
update sud (x,y) newValue = 
                Sudoku (rows sud !!= (x,(rows sud !! x) !!= (y,newValue)))
				
-- solve returns a solved sudoku or Nothing if no solution is found.
solve :: Sudoku -> Maybe Sudoku
solve sud 
   | not (isOkay sud) = Nothing
   | isSolved sud     = Just sud
   | otherwise        = listToMaybe.catMaybes $
        [solve (candidate i) | i <- [1..9], isOkay (candidate i)]
           where candidate i  = update sud (blank sud) (Just i)
   
-- ReadAndSolve reads a sudoku from a file and prints 
-- the solution if found. 
readAndSolve :: FilePath -> IO ()
readAndSolve filePath =
   do
      sud <- readSudoku filePath
      let solvedSud = solve sud
      if isJust solvedSud
         then
            printSudoku $ fromJust solvedSud
         else
            putStrLn "(no solution)"

--isSolutionOf checks if a sudoku is a valid solution to another sudoku
--i.e the solution is "okay"  and all digits in the original sudoku
--also appear in the same place in the solution.
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol sud =
   (isOkay sol)
   && 
   (
   and $ zipWith 
      (\x y -> x==y || isNothing y) 
      (concat (rows sol)) 
      (concat (rows sud))
   )
 
-------------------------------------------------------------------------
-- QuickCheck--
-------------------------------------------------------------------------

-- A datatype that will be used to generate random
-- positions
data CheckPos = CheckPos (Int,Int)
   deriving (Show)

-- A datatype that will be used to generate Ints
-- between 0 and 8
data RangedInt = RangedInt Int
   deriving (Show)
   
-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
   arbitrary =
      do rows <- sequence [sequence [cell | j <- [1..9]] | i <- [1..9]]
         return (Sudoku rows)

-- tells quickCheck how to generate Positions (Pos)
instance Arbitrary CheckPos where
   arbitrary =
      do
         RangedInt int1 <- arbitrary :: Gen RangedInt
         RangedInt int2 <- arbitrary :: Gen RangedInt
         return (CheckPos (int1, int2) )

-- tells quickCheck how to generate a ranged Int
instance Arbitrary RangedInt where
   arbitrary =
      do
         int <- choose (0,8)
         return (RangedInt int)


-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency 
   [
     (8, return Nothing )
   , (1, do r <- choose (1,9); return (Just r))
   ]

-- test if a generated Sudoku is valid
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud

-- prop_BlockWorking 
-- for each Sudoku, there are 3 * 9 blocks, and each block 
-- has exactly 9 cells.
prop_BlocksWorking :: Sudoku -> Bool
prop_BlocksWorking sud = 
   length blockLs == 27 && and [ length block == 9 | block <- blockLs ]
      where blockLs = (blocks sud)

-- prop_IsBlank 
-- checks if blank actually returns the position of a blank cell
prop_IsBlank :: Sudoku -> Bool
prop_IsBlank sud = (row !! (fst cell)) !! (snd cell) == Nothing
   where cell    = blank sud
         row     = rows sud

-- prop_InsertCorrectPlace checks if the !!= operator works as expected
prop_InsertCorrectPlace :: Eq a => [a] -> (RangedInt,a) -> Property
prop_InsertCorrectPlace ls (RangedInt cell, content) =
   (cell <= length ls) ==>
      ((ls !!= (cell,content)) !! cell == content)

-- prop_UpdateWorking checks if update is working as expected
-- by updating an empty sudoku at a random position, and
-- then checking if that position is actually updated correctly.
prop_UpdateWorking :: CheckPos -> RangedInt -> Bool
prop_UpdateWorking (CheckPos pos) (RangedInt testCell) = 
   (Just testCell) == cell (update allBlankSudoku pos (Just testCell)) pos
      where cell sud (x,y) = (rows sud !! x) !! y

-- prop_SolveSound checks if solve produces a valid solution using 
-- isSolution of.  Only "okay" sudokus are checked
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = 
   isOkay sud ==> isSolutionOf ( fromJust(solve sud) ) sud

fewerCheck prop = quickCheckWith stdArgs{ maxSuccess = 30 } prop

-------------------------------------------------------------------------