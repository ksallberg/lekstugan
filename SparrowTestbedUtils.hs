import Control.Monad      (forM_)
import System.Directory   (getDirectoryContents)
import System.Environment (getArgs)
import System.IO

{-- Used for storing information for the input.conf files --}
data Setting = Setting
  {
    theId ::Integer
  , probes::Integer
  , nodes ::Integer
  , jobs  ::Integer
  }

{- toString definition for Setting -}
instance Show Setting where
   show setting = "OUTFILE:output"    ++ (show (theId setting))  ++".out\n"++
                  "NUMBER_OF_PROBES:" ++ (show (probes setting)) ++"\n"    ++
                  "NUMBER_OF_NODES:"  ++ (show (nodes setting))  ++"\n"    ++
                  "NUMBER_OF_JOBS:"   ++ (show (jobs setting))

{-
   If no arguments are given, Print all combinations 
   for the setting variables probes, nodes, jobs and their ID.
-}
main :: IO ()
main = do
   args <- getArgs
   case args of
      [] -> createSettingFiles
      -- for all files in a given directory, do performOutputParsing and
      -- save the result to appendfile.txt
      _  -> do outputFiles <- getDirectoryContents (head args)
               forM_ [(head args)++o|o<-outputFiles,o/="."&&o/=".."]
                     ((flip performOutputParsing) "appendfile.txt")
               
{- Generate combinations needed to test and write all files to disk -}
createSettingFiles :: IO ()
createSettingFiles = do
   let numberOfProbes = [1,2,3,5,10]
       numberOfNodes  = [100,1000,10000,100000]
       numberOfJobs   = [1000,10000,100000,1000000,10000000]
       combinations   = [(probe,node,job)|probe<-numberOfProbes,
                                          node<-numberOfNodes,
                                          job<-numberOfJobs]
       idcomb         = [Setting theId probe node job
                        | (theId,(probe,node,job)) <- zip [1..] combinations]
       fileName       = "resourcemanager/settingfiles/input"
   forM_ idcomb $ \x-> writeFileLine (fileName++(show . theId) x++".conf")
                                     (show x)
                                     WriteMode

{- Create a file, write the str given and close the file -}
writeFileLine :: FilePath -> String -> IOMode ->IO ()
writeFileLine fp content mode = do
   handle <- openFile fp mode
   hPutStrLn handle content
   hClose handle

{- Read an output file, parse it, 
   perform calculations and generate new output files
-}
performOutputParsing :: FilePath -> FilePath -> IO ()
performOutputParsing readFrom writeTo = do
   file <- readFile readFrom
   writeFileLine writeTo "test todo fill in read content" AppendMode
