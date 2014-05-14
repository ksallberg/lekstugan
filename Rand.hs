import System.Random

main :: IO ()
main = do putStrLn "testing random"
          randGen <- getStdGen
          let (theResult,newSeed) = theRandomInt randGen
          putStrLn $ "random value: " ++ show theResult

theRandomInt :: StdGen -> (Integer,StdGen)
theRandomInt gen = randomR (0,100) gen
