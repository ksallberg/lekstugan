import Control.Applicative

-- pure :: a -> f a

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

test1 = pure (+1) <*> [1,2,3,4]
test2 = pure (+1) <*> Just 4

-- (*>) :: f a -> f b -> f a

-- (<*) :: f a -> f b -> f b

-- liftA :: Applicative f => (a -> b) -> f a -> f b

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
