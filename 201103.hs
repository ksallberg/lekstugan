import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

newtype DList a = DL {unDL :: [a] -> [a]}

append :: DList a -> DList a -> DList a
append x y = undefined--DL $ \_ -> unDL x  unDL y

---------------------------------

-- problem 2

-- a
{-
In computer science, denotational semantics (initially known as mathematical
semantics or Scott–Strachey semantics) is an approach of formalizing the
meanings of programming languages by constructing mathematical objects
(called denotations) that describe the meanings of expressions from
the languages.
-}

{-
Operational semantics are a category of formal programming language semantics
in which certain desired properties of a program, such as correctness,
safety or security, are verified by constructing proofs from logical statements
about its execution and procedures, rather than by attaching mathematical
meanings to its terms (denotational semantics). Operational semantics are
classified in two categories: structural operational semantics (or small-step
semantics) formally describe how the individual steps of a computation take
place in a computer-based system. By opposition natural semantics
(or big-step semantics) describe how the overall results of the executions
are obtained. Other approaches to providing a formal semantics of programming
languages include axiomatic semantics and denotational semantics.
-}

-- b

newtype Gold = Gold Int
type Balance = TVar Gold

-- Transfer some money from balance1 (from) to balance2 (to).
-- If balance1 is out of money, then
transfer :: Gold -> Balance -> Balance -> IO ()
transfer (Gold gold) from to =
   atomically $ do
       (Gold oldFrom) <- readTVar from
       case oldFrom - gold < 0 of
           -- Quick! Take a loan
           True  -> retry
           False -> do (Gold oldTo)   <- readTVar to
                       writeTVar from (Gold $ oldFrom - gold)
                       writeTVar to   (Gold $ oldTo + gold)

test :: IO ()
test = do balance1 <- atomically $ newTVar (Gold 1000)
          balance2 <- atomically $ newTVar (Gold 200)
          putStrLn "balance2 pays 500 gold to balance1 because of a car deal!"
          putStrLn "balance2 cant afford this expensive car, only has 200 gold"
          -- new "loan" process...
          forkIO $ do putStrLn ("Take loan in 1000 microsecs, to make the" ++
                                "transfer retry")
                      threadDelay 2000000
                      putStrLn "loan taken"
                      atomically $ takeLoan balance2
          transfer (Gold 500) balance2 balance1
          newVal1 <- doRead balance1
          newVal2 <- doRead balance2
          putStrLn ("New balance1: " ++ show newVal1)
          putStrLn ("New balance2: " ++ show newVal2)

-- take a 2000 gold loan
takeLoan :: Balance -> STM ()
takeLoan balance = do
    (Gold oldBalance) <- readTVar balance
    writeTVar balance (Gold $ oldBalance + 2000)

doRead :: Balance -> IO Int
doRead b = atomically $ do (Gold val) <- readTVar b
                           return val

-- c

{-
    The system waits because there is no point in retrying if the value does
    not change. The transaction is still negative/illegal.

    Yes, if the new value is negative we will try again.
-}
