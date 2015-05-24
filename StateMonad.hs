import Control.Monad
import Control.Monad.State

---- :::: Native state monad :::: ----

-- Run the native State monad with a
-- simple integer value
run :: Int -> IO ()
run input = do putStrLn "Let's use states!"
               let (a,s) = runState doSomething input
               putStrLn $ "Return value: " ++ show a
               putStrLn $ "Final state: " ++ show s

-- Increment the value of the native
-- state monad
doSomething :: State Int ()
doSomething = do i <- get
                 let new = i + 10
                 put new
                 return ()

---- :::: Own state monad implementation :::: ----

newtype MyState s a = MyState {myRunState :: s -> (a,s)}

instance Monad (MyState s) where
   return x           = MyState (\s -> (x,s))
   (MyState ol) >>= f = MyState $ \s -> let (n, new) = ol s
                                            (MyState g) = f n
                                        in  g new
   fail x             = error "error hehe"

myGet :: MyState a a
myGet = MyState $ \s -> (s,s)

myPut :: Int -> MyState Int ()
myPut input = MyState $ \s -> ((),input)

myDoSomething :: MyState Int ()
myDoSomething = do i <- myGet
                   let new = i + 10
                   myPut new
                   return ()

myRun :: Int -> IO ()
myRun input = do putStrLn "Let's use my state!"
                 let (a,s) = myRunState myDoSomething input
                 putStrLn $ "My return value: " ++ show a
                 putStrLn $ "My final state: "  ++ show s
