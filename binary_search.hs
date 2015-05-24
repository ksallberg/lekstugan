{-
A data type for creating a binary tree, and some functions for inserting,
removing and searching for nodes.

Usage: testTree = foldr addNode Empty [10,20,12,33,44,0]
    (Add some integers to the tree)
testTree2 = foldr removeNode testTree [33,44,0]
    (Remove some from the first tree)
-}

-- Data type for representing a tree
data BinTree a = Empty | Node a (BinTree a) (BinTree a)
   deriving (Eq, Show, Read)

-- Add a node to a tree
addNode :: Eq a => Ord a => a -> BinTree a -> BinTree a
addNode toAdd Empty = (Node toAdd Empty Empty) -- add something in this spot
addNode toAdd tree@(Node x l r) | toAdd == x = tree -- toAdd is already
                                                    -- in the tree
                                | toAdd > x  = Node x l (addNode toAdd r)
                                | otherwise  = Node x (addNode toAdd l) r

-- Add a tree to a tree
addTree :: Ord a => BinTree a -> BinTree a -> BinTree a
addTree Empty t2        = t2
addTree (Node x l r) t2 = addTree r (addTree l (addNode x t2))

-- Remove a node from a tree
removeNode :: Ord a => a -> BinTree a -> BinTree a
removeNode toRem Empty = Empty
removeNode toRem (Node x l r) | toRem == x = addTree l r
                              | toRem > x  = Node x l (removeNode toRem r)
                              | otherwise  = Node x (removeNode toRem l) r

-- Search for an element in the tree
get :: Ord a => a -> BinTree a -> Maybe a
get elem Empty = Nothing
get elem (Node x l r) | elem == x = Just x
                      | elem > x  = get elem r
                      | otherwise = get elem l

-- Get the yield of a tree
preOrderTraverse :: BinTree a -> [a]
preOrderTraverse Empty = []
preOrderTraverse tree@(Node x l r) = x : (preOrderTraverse l ++
                                          preOrderTraverse r)
