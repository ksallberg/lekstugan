import Control.Applicative

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

-- (<*) :: f a -> f b -> f b

-- liftA :: Applicative f => (a -> b) -> f a -> f b
testA1 :: [Int]
testA1 = liftA (+1) [1,2,3,4]

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

