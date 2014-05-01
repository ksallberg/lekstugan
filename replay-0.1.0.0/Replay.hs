-- | This module contains a replay monad. A replay monad can
--   decide to stop in the middle of a running computation,
--   and then later pick up where it left of.
--   
--   We have a shallow implementation based around the semantics of
--   the Replay type.
--
module Replay (

    -- | Types
      Replay (..)
    , Trace
    , Item (..)
    
    -- | Operations
    , liftIO
    , ask
    , emptyTrace
    , addAnswer

    -- | Run function
    , run

) where

-- | Type representing Replay
--
--   Trace r -> Trace r -> IO
--
--   Here, the first trace is used to do retracing. We pick
--   Items from it until it's empty. When it's empty, retracing mode
--   is complete. 
--
--   The second trace is supposed to contain all traces from the
--   start, and is then expanded with later computations
--
--   In the IO return value, the first item in the tuple is a trace
--   that will be read from, and the second trace is to keep the history.
newtype Replay q r a = 
    Replay {runReplay :: Trace r -> Trace r ->
                         IO (Trace r, Either q (a,Trace r))}


-- | Type representing a trace of
--   computations previously occured
--
--   Item defined what can happen in a computation
type Trace r = [Item r]

data Item r = Answer r
            | Result String
   deriving (Show,Read)

-- Operations

-- | Monad implementation 
--
--   In the case of return, a monad computation is 
--   finished (completed) and we return the final replay
--   wrapped in the IO monad.
--
--   In the case of the bind definition, we transition
--   from the old Replay value to a new one, carrying data
--   from the old state into the new state.
--   
--   We grab the new state out of it's IO monad wrapper.
--      
--      If the state is of Right, then it's time to
--      calculate the next step using the (a-> Replay b) function
--          
--      If the state is Left, then it's time to break the 
--      chain and return the question produced by running "a r1 r2"
--
instance Monad (Replay q r) where
    return a       = Replay $ \r1 r2 -> return (r2, Right (a,r2))
    Replay a >>= f = Replay $ \r1 r2 -> do
        result <- a r1 r2
        case result of
            (r', Right (a',rem)) -> (runReplay (f a')) rem r'
            (r', Left q') -> return $ (r', Left q')

-- | Lifting function to perform IO actions and store the result
--   of them in our trace. 
--
--   the first thing we do is checking the trace to see if an 
--   action has already been performed
--   
--   We are running split trace on the trace to pick from,
--   and then matching to catch the different cases that can happen,
--   The main idea is to read or show a part of the trace
--
liftIO :: (Show a, Read a) => IO a -> Replay q r a
liftIO f = Replay $ \r1 r2 ->
    case splitTrace r1 of
        (Just [Result x], Just rem) -> return (r2, Right ((read x),rem))
        (Just [Result x], Nothing)  -> return (r2, Right ((read x),emptyTrace))
        (Nothing,         _ ) -> 
            do a <- f -- picking x out of the IO monad
               return (addResult r2 (show a), Right (a,emptyTrace ))

-- | The ask fuction uses roughly the same process as liftIO.
--
--   If a question has already been answered in the trace, then
--   we just return a Right, causing the program to go forward.
--
--   If the question hasn't been answered, we return a Left, 
--   causing the program to interrupt with a question
ask :: q -> Replay q r r
ask q = Replay $ \r1 r2 -> 
    case splitTrace r1 of
        (Just [Answer x], Just rem) -> return (r2, Right (x,rem))
        (Just [Answer x], Nothing)  -> return (r2, Right (x,emptyTrace))
        (Nothing        , _)        -> return (r2, Left q)
                           
emptyTrace :: Trace r
emptyTrace = []

addAnswer :: Trace r -> r -> Trace r
addAnswer t r = t ++ [Answer r] 

-- | Because we are using a shallow implementation, 
--   not muh work is done here. We feed the given replay 
--   with a trace and return the result of that run
run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run replay ls = do result <- (runReplay replay) ls ls
                   case result of
                       (r', Left q)     -> return $ Left (q,r')
                       (_, Right (a,_)) -> return $ Right a
                   

-- | Add a string to a trace
--     
--   Note that we are using a String as an intermediary storage type
--   this is because we have to be able to convert between a and r
addResult :: Trace r -> String -> Trace r
addResult t a = t ++ [Result a]


splitTrace :: Trace a -> (Maybe (Trace a), Maybe (Trace a))
splitTrace t = (safeHead t, safeTail t)

-- | Allowing us to handle the cases of failing
--   head operations on empty lists
safeHead :: Trace a -> Maybe (Trace a)
safeHead []     = Nothing
safeHead (x:xs) = Just [x]

-- | Same as above but for failing tails
safeTail :: Trace a -> Maybe (Trace a)
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (x:xs) = Just xs

