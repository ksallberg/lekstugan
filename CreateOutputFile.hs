import Control.Monad

data Setting = Setting {
   theId::Integer,
   probes::Integer,
   nodes::Integer,
   jobs::Integer
   }

instance Show Setting where
   show setting = "OUTFILE:output"    ++ (show (theId setting))  ++".out\n"++
                  "NUMBER_OF_PROBES:" ++ (show (probes setting)) ++"\n"    ++
                  "NUMBER_OF_NODES:"  ++ (show (nodes setting))  ++"\n"    ++
                  "NUMBER_OF_JOBS:"   ++ (show (jobs setting))
{-
   Print all combinations for the setting variables
   probes, nodes, jobs and their ID.
-}
main :: IO ()
main = do let numberOfProbes = [1,2,3,5,10]
              numberOfNodes  = [100,1000,10000,100000]
              numberOfJobs   = [1000,10000,100000,1000000,10000000]
              combinations   = [(probe,node,job)|probe<-numberOfProbes,
                                                 node<-numberOfNodes,
                                                 job<-numberOfJobs]
              idcomb         = [Setting theId probe node job
                               | (theId,(probe,node,job)) <- zip [1..] combinations]
              fileName       = "resourcemanager/settingfiles/input"
          forM_ idcomb (putStrLn . show)
