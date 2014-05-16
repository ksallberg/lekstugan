import Control.Monad      (forM_)
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
      _  -> performOutputParsing $ head args

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
   forM_ idcomb (\x-> writeSettingFile (fileName++(show . theId) x++".conf")
                                       (show x))

{- Create a file, write the str given and close the file -}
writeSettingFile :: FilePath -> String -> IO ()
writeSettingFile fp content = do
   handle <- openFile fp WriteMode
   hPutStrLn handle content
   hClose handle

{- Read an output file, parse it, 
   perform calculations and generate new output files
-}
performOutputParsing :: FilePath -> IO ()
performOutputParsing fp = do 
   file <- readFile fp
   putStrLn $ "File read:\n" ++ file
