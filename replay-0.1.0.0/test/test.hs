import Data.IORef
import Replay
import System.Exit

-- | Runs the test suite for the replay library
main :: IO ()
main = do
  results <- runTests
  if and results
    then return ()
    else exitFailure

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = IO () -> Replay () Int Int

-- | A result is a pair of the final result of the program
--   and the number of 'ticks' executed.
type Result  = (Int, Int)
type Input   = [Int]

-- | A test case collects a program and a list of answers together
--   with its expected result.
data TestCase = TestCase
  { testName    :: String
  , testInput   :: Input
  , testResult  :: Result
  , testProgram :: Program
  }

-- | Running a program.
runProgram :: Program -> Input -> IO Result
runProgram p inp = do
    counter <- newIORef 0
    let tick = modifyIORef counter (+1)
    x <- play (p tick) emptyTrace inp
    n <- readIORef counter
    return (x, n)
  where
    play prog t inp = do
      r <- run prog t
      case r of
        Right x      -> return x
        Left (_, t') -> case inp of
          []       -> error "too few inputs"
          a : inp' -> play prog (addAnswer t' a) inp'

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> IO Bool
checkTestCase (TestCase name i r p) = do
  putStr $ name ++ ": "
  r' <- runProgram p i
  if r == r'
    then putStrLn "ok" >> return True
    else putStrLn ("FAIL: expected " ++ show r ++
                  " instead of " ++ show r')
         >> return False


-- | List of interesting test cases.
testCases :: [TestCase]
testCases =
  [ TestCase
    { testName    = "test1"
    , testInput   = [3,4]
    , testResult  = (8, 1)
    , testProgram = \tick -> do
        liftIO tick
        a <- ask () -- should be 3
        b <- liftIO (return 1)
        c <- ask () -- should be 4
        return (a + b + c)
    },
    -- | Testing that getting (0,0) as a result works
    --   by sending input and then not using that input
    TestCase
    { testName    = "test2"
    , testInput   = [1,1,2]
    , testResult  = (0,0)
    , testProgram = \tick -> do
        return (0)
    },
    -- | Testing 5 ticks, works?
    TestCase
    { testName    = "test3"
    , testInput   = []
    , testResult  = (0,5)
    , testProgram = \tick -> do
        liftIO tick
        liftIO tick
        liftIO tick
        liftIO tick
        liftIO tick
        a <- liftIO (return 0)
        return a
    },
    -- | Testing to add 10 1's
    --   and check that we get the right
    --   value back
     TestCase
    { testName    = "justOnes"
    , testInput   = [ 1 | x <- [1..10]]
    , testResult  = (10, 1)
    , testProgram = \tick -> do
        y <- liftIO tick
        a <- ask ()
        b <- ask ()
        c <- ask ()
        d <- ask ()
        e <- ask ()
        f <- ask ()
        g <- ask ()
        h <- ask ()
        i <- ask ()
        z <- liftIO (return 1)
        j <- ask ()

        return (a+b+c+d+e+f+g+h+i+j)
    },
    -- | Corner case: when input is empty,
    --   the result should be (0,0)
    TestCase
    { testName    = "empty"
    , testInput   = []
    , testResult  = (0, 0)
    , testProgram = \tick -> do

        return 0
    },
    -- | Check that we can do ticks
    --   and return something else
    --   than 0 even though the input 
    --   list is empty
    TestCase
    { testName    = "emptyReturn"
    , testInput   = []
    , testResult  = (8,8)
    , testProgram = \tick -> do
      liftIO tick 
      liftIO tick
      liftIO tick
      liftIO tick
      liftIO tick
      liftIO tick
      liftIO tick
      liftIO tick
      return 8
    },
    -- | Empty input, one tick and return 0
    TestCase
    { testName     = "emptyTick"
    , testInput    = []
    , testResult   = (0,1)
    , testProgram  = \tick -> do
              a <- liftIO tick
              return 0
    },
    -- | Testing what happens with an IO operation embedded
    TestCase
    { testName    = "liftIO getLine"
    , testInput   = []
    , testResult  = (0,0)
    , testProgram = \tick -> do
        a <- (liftIO $ putStrLn "bepa")
        return 0
    }
  ]

-- | Running all the test cases.
runTests = mapM checkTestCase testCases
