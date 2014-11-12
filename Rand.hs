import System.Random

main :: IO ()
main = do putStrLn "testing random"
          randGen <- getStdGen
          let (theResult,newSeed) = theRandomInt randGen
          let (theResult2,_)      = theRandomInt newSeed
          putStrLn $ "random value: "  ++ show theResult
          putStrLn $ "random value2: " ++ show theResult2

theRandomInt :: StdGen -> (Integer,StdGen)
theRandomInt gen = randomR (0,100) gen
