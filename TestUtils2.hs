module TestUtils2 where

-- runhaskell TestUtils.hs resourcemanager/output/
import Control.Exception
import Control.Monad         (forM_)
import Control.Monad.Error
import Data.Either
import Data.List
import Data.Maybe
import Data.Tuple
import qualified Data.Map    as M
import System.Directory      (getDirectoryContents)
import System.Environment    (getArgs)
import System.IO
import System.FilePath.Posix (takeFileName)

-- list the commands
data Command    = PRB  | INI | ASN | SCH deriving (Show,Read,Ord,Eq)
data End        = High | Low             deriving (Show,Read,Eq)
type TimeStamp  = Integer
type JobId      = Integer
type Output     = M.Map Integer [Measure] -- used for saving id [(cmd,time)]
type Measure    = (Command, TimeStamp)
type OutputLine = (JobId,   Measure)
type TestSpec   = [(String,((Command,End),(Command,End)))]

-- the specification of what should be calculated
test1 :: TestSpec
test1 = [(,) "Probing"        $ (,) (PRB,Low)  (INI,Low),
         (,) "NetworkLatency" $ (,) (ASN,High) (PRB,Low),
         (,) "WaitingQueue"   $ (,) (SCH,High) (ASN,Low),
         (,) "Total"          $ (,) (SCH,High) (INI,Low)]

test2 :: TestSpec
test2 = [(,) "NetworkLatency" $ (,) (ASN,High) (INI,Low),
         (,) "WaitingQueue"   $ (,) (SCH,High) (ASN,Low),
         (,) "Total"          $ (,) (SCH,High) (INI,Low)]

{- Expects java output files directory, and test1/test2 as spec -}
main :: IO ()
main = do
   [outfileDir,testCase] <- getArgs
   outputFiles <- getDirectoryContents outfileDir
   let skipFiles = [".","..",".DS_Store"]
       spec      = case testCase of
                      "test1" -> test1
                      "test2" -> test2
   forM_ [outfileDir++o|o<-outputFiles,not $ elem o skipFiles]
         (\file->performOutputParsing spec file "resourcemanager/statistics/")

{- This is the workhorse that dispatches all calculations and handles errors -}
performOutputParsing :: TestSpec -> FilePath -> FilePath -> IO ()
performOutputParsing spec from to = catch
   (do file     <- readFile from
       allLines <- sequence [runErrorT $ parseLine str|str<-lines file]
       -- do the statistics calcs
       let theMap = foldl (+->) M.empty $ rights allLines
           ls     = map ((,) spec) $ sort $ M.toList theMap
           fName  = takeFileName from
           calcs  = (,) (foldl calculations "" ls) (to++fName)
           avgs   = (,) (averages ls)              (to++"-averages-"++fName)
       -- print files (calcs and averages)
       writeFileLine (snd calcs) (fst calcs) WriteMode
       writeFileLine (snd avgs)  (fst avgs)  WriteMode)
   ((putStrLn . show) :: ErrorCall -> IO ()) -- handler

{- Parsing a line and may throw ErrorT errors depending on the file input -}
parseLine :: String -> ErrorT String IO OutputLine
parseLine inp = do
   case (length (words inp) >= 3) of
      False -> throwError "Too few words at a line"
      True  -> do let [command,jobId,timest] = words inp
                  jid <- testFail "no jobid"   (reads jobId   :: [(Integer,  String)])
                  com <- testFail "no command" (reads command :: [(Command,  String)])
                  tim <- testFail "no timest"  (reads timest  :: [(TimeStamp,String)])
                  return (jid,(com,tim))
   where testFail :: String -> [(a,String)] -> ErrorT String IO a
         testFail err [] = throwError err
         testFail _   ls = return $ (fst . head) ls

{- !!!!!! Below are utility functions !!!!!!!-}

{- Create a file, write the str given and close the file -}
writeFileLine :: FilePath -> String -> IOMode -> IO ()
writeFileLine fp content mode = do
   handle <- openFile fp mode
   hPutStrLn handle content
   hClose handle

{- Here we can perform the calculations needed, and then format it all as
   a string -}
calculations :: String -> (TestSpec,(JobId,[Measure])) -> String
calculations old (spec,(jobId,commLs)) = old ++ show jobId ++ "," ++ combs ++ "\n"
   where combs = concat [(toStr x y commLs) ++ "," | (x,y)<-(map snd spec)]
         toStr (c1,e1) (c2,e2) cs = show $ (apply c1 e1 cs) - (apply c2 e2 cs)
         apply c e cs             = snd (getM c e cs)

{- Get averages and 99th percentile as a String -}
averages :: [(TestSpec,(JobId,[Measure]))] -> String
averages ls =
   concat [label++" averages time "     ++(show $ getAvg $ timesFor f t (map snd ls))++
           "\n" ++label++" 99th p time "++(show $ get99P $ timesFor f t (map snd ls))++"\n"
          | (label,(f,t)) <- (fst (head ls))]

timesFor :: (Command,End) -> (Command,End) -> [(JobId,[Measure])] -> [TimeStamp]
timesFor (c1,e1) (c2,e2) ls = [snd m1 - snd m2 | (m1,m2) <- a ]
   where a = [(getM c1 e1 meas,getM c2 e2 meas)|(_,meas) <- ls]

{- Try to get a measure for a command from a list of measures -}
getM :: Command -> End -> [Measure] -> Measure
getM cmd e ls =
   case (isJust $ lookup cmd $ searchList cmd e ls) of
      True  -> head $ searchList cmd e ls
      False -> error $ "getM: For command "++(show cmd)++
                       " there was no measure in the commandList of "++(show ls)

{- search for a command and rank by End (High or Low) -}
searchList :: Command -> End -> [Measure] -> [Measure]
searchList cmd rank inp | rank == High = reverse ls
                        | otherwise    = ls
   where ls = map swap $ sort [swap x|x<-inp,fst x==cmd]

{- For a list of times, calculate average -}
getAvg :: [TimeStamp] -> TimeStamp
getAvg ls = div (sum ls) (toInteger $ length ls)

{- For a list of times, calculate 99th percentile
@see https://answers.yahoo.com/question/index?qid=1005122102489 -}
get99P :: [TimeStamp] -> TimeStamp
get99P ls = last percent99
   where percent99 = take (ceiling $ len*0.99) (sort ls)
         len       = fromIntegral  $ length    (sort ls)

{- add one item to the output hashmap
   fst line is the jobId, snd line are the measures for the outputline -}
(+->) :: Output -> OutputLine -> Output
out +-> line = M.insertWith (++) (fst line) [snd line] out
