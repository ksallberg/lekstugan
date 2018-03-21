{-
   Name: Kristian Sällberg
   Matriculation number: 2057128S
   Course: Functional Programming 4
   Ex title: JPEG Manipulation in Haskell
   Date: 2012-11-29
-}

{-
   Status report:
   I did solved questions 1, 2 and 3. I'm not aware of any bugs.
   Everything works but the checksum calculation is a bit slow.

   I'm using a package called exif-3000.0.0 from hackage
   for reading exif data:
   http://hackage.haskell.org/package/exif-3000.0.0

   I read about how to convert integers to hex here:
   http://stackoverflow.com/questions/1959715/how-
   to-print-integer-literals-in-binary-or-hex-in-haskell

   The handout clearly stated this program should not take
   any input. So I defined the list of files to process as
   the function "files" below. This requires the pictures to
   be in a folder called pics.

   I'm a but curious to if the function "pickOut" is really needed.
   It feels like I might be doing something wrong there?
   Going from IO [IO String] -> IO [String] seems a bit awkward
   when having to do it manually like this.

   The handout says the thumbnail should be 128 px wide. I also
   assume the height should be 128px.

   I used http://hackage.haskell.org/packages/archive/
   gd/3000.4.0/doc/html/Graphics-GD.html
   to do the actual jpeg size manipulation. It's super easy
   to use.

   This was a very fun assignment! ^_^
-}

module JpegManip where

import Data.Char
import Data.List
import Data.Maybe
import Graphics.Exif
import qualified Data.ByteString.Char8 as C
import Numeric
import Graphics.GD

-- list the files to load
files = [ "pics/pic" ++ genFileName x ++ ".jpeg" | x <- [1..2] ]

-- generate the correct file name
genFileName :: Int -> String
genFileName input | input < 10 = "0" ++ show input
                  | otherwise  = show input

{-
   main to run the different questions
-}
main :: IO ()
main = do
  putStrLn "Printouts for Question1 and 2: "
  printThem (sortAndCouple (getTags files) files)
  putStrLn "Start creating thumbnails!"
  createThumbs files

---------- Question 1 functions:

{-
   load the files, for each file, load all tags and then
   keep the one called DateTimeDigitized
-}
getTags :: [FilePath] -> IO [String]
getTags fp = do
  fhlist <- sequence [fromFile file | file <- fp] :: IO [Exif]
  tags <- sequence [getTag exif "DateTimeDigitized" |
                    exif <- fhlist] :: IO [Maybe String]
  putStrLn $ "hej: " ++ show tags
  return $ map fromJust tags

{-
   Receive a list of dates, a list of filenames and put the
   content as a (name, date) tuple. Then sort and put in a monad
-}
sortAndCouple :: IO [String] -> [FilePath] -> IO [(String,String)]
sortAndCouple list fileNames = do
  ls <- list
  let lsCoupled = zip ls fileNames
  let lsSorted  = sort lsCoupled
  return lsSorted

{-
   print a list of (String,String) by lifting them from their
   monad and doing IO. do it recursively til the list is []
-}
printThem :: IO[(String,String)] -> IO ()
printThem list = do
  ls <- list
  case ls of
    [] ->
      putStrLn "___"
    _ -> do
      putStr         $ "file: " ++ snd (head ls) ++
        ", time: " ++ fst (head ls)
      putStr         $ ", checksum: "
      calcCheckSum   $ snd (head ls)
      printThem      $ return (tail ls)

---------- Question 2 functions:

{-
   Calculate the checksum for a file
-}
calcCheckSum :: FilePath -> IO ()
calcCheckSum str = do
  file <- C.readFile str
  putStrLn $ showIntAtBase 16 intToDigit (eatBytes file) ""

{-
   At first I had a really complex function for this.

   I ran unpack to get a String back, then I converted each
   char to an Int, then I summed everything. It usually took
   1 min 11 secs to calculate the checksum for a picture.
   O(n^3) or something I think.

   Now I just sum everything from the beginning of the
   ByteString/list. With this approach it usually takes
   12 seconds (1 min better!). O(n) now?
-}
eatBytes :: C.ByteString -> Int
eatBytes xs | C.null xs = 0
            | otherwise = ( ord (C.head xs) ) + eatBytes (C.tail xs)

------------- Question 3 functions:

{-
   Iterate through the files and create thumbs for
   all of them
-}
createThumbs :: [FilePath] -> IO ()
createThumbs [] = putStrLn "Thumbs created!"
createThumbs (x:xs) = do
  createThumb x
  createThumbs xs

{-
   Use Graphics.GD to load a picture, resize it and
   save it as a thumbnail
-}
createThumb :: FilePath -> IO ()
createThumb fpath = do
  file <- loadJpegFile fpath
  let outputName = ("pics/" ++ "tn_"
                    ++ dropFromFileName fpath ++ ".jpeg")
  resized <- resizeImage 128 128 file
  saveJpegFile 95 outputName resized
  putStrLn $ "Thumb created: " ++ outputName

{-
   Just remove everything from the file name that's not
   part of it's name when put as "blabal/name.blabla"

   dropFromFileName "blabla/name.blalba" => name
-}
dropFromFileName :: String -> String
dropFromFileName str =
  takeWhile (\x->x/='.') $ drop 1 $ dropWhile (\x->x/='/') str
