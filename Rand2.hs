-- Random wrapped in State Monad

import System.Random
import Control.Monad.State

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom = do gen <- get
               let (val, gen') = random gen
               put gen'
               return val

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do oldState <- getStdGen
                   let (result, newState) = runState getTwoRandoms oldState
                   setStdGen newState
                   return result

main :: IO ()
main = do a <- runTwoRandoms
          b <- runTwoRandoms
          c <- runTwoRandoms
          mapM_ putStrLn (map show [a, b, c])
