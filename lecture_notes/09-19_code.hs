data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq)

-- Type signature
-- Leaf :: a -> Tree a

treeExample :: Tree Int
treeExample = Node (Node (Leaf 1) (Node (Leaf 4) (Leaf 5))) (Node (Leaf 3) (Leaf 5))

treeSum :: Num a => Tree a -> a
treeSum (Leaf x) = x
treeSum (Node l r) = treeSum l + treeSum r

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Node t1 t2) = Node (treeMap f t1) (treeMap f t2)
-- The Node constructor handles rebuilding the tree structure

treeFold :: (a -> b) -> (b -> b -> b) -> Tree a -> b
treeFold fLeaf fNode (Leaf x) = fLeaf x
treeFold fLeaf fNode (Node t1 t2) = fNode 
                                        (treeFold fLeaf fNode t1) 
                                        (treeFold fLeaf fNode t2)

treeFold id (+) treeExample

enumerate :: Tree a -> [a]
enumerate = treeFold (\n -> [n]) (++)