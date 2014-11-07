import Control.Applicative
import Control.Monad
import Data.Maybe

-- Applicative bra för att jobba med dom här boxxade sakerna
combo = (fmap (++) ["ha", "heh", "hmm"]) <*> ["?", "!", "."]
combo2 = (++) <$> ["ha", "heh", "hmm"] <*> ["?", "!", "."]

-- pure :: a -> f a
testPure :: Maybe Int
testPure = pure 23

testPure2 :: [Int]
testPure2 = pure 34

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
test1 :: [Int]
test1 = pure (+1) <*> [1,2,3,4]

test2 :: Maybe Int
test2 = pure (+1) <*> Just 4

-- (*>) :: f a -> f b -> f a
rightPenguine :: [Int]
rightPenguine = [1] *> [2]

-- (<*) :: f a -> f b -> f b
leftPenguine :: [Int]
leftPenguine = [4] <* [5]

-- liftA :: Applicative f => (a -> b) -> f a -> f b
testA1 :: [Int]
testA1 = liftA (+1) [1,2,3,4]

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
testA2 :: Maybe Int -> Maybe Int -> Maybe Int
testA2 a b = liftA2 (+) a b

-- still applicative
composeA2 :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
composeA2 a b c = testA2 (testA2 a b) c

-- testing with monad
composeMonad :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
composeMonad a b c = do let x = testA2 a b
                        y <- testA2 x c
                        return (20 + y)

testComposeA2 :: Maybe Int
testComposeA2 = composeA2 (Just 3) (Just 3) (Just 4)

testComposeMonad :: Maybe Int
testComposeMonad = composeMonad (Just 3) (Just 3) (Just 4)

tert :: Int -> Int -> Int -> Int
tert a b c = a + b - c

-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
testA3 :: Maybe Int
testA3 = liftA3 tert (Just 33) (Just 2) (Just 45)
