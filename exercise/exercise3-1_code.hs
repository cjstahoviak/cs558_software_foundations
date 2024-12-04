-- 3.1 Evaluation
-- For each of the following well-typed expressions, provide a correct type and then determine
-- whether the expression has a value, and, if not, whether it loops forever, or raises an error. For
-- each expression, either give its value, or write “diverges” if it loops forever, or else write “fails”.
-- (If the value is a function, just write “a function”.)

-- • \n -> n ‘div‘ 0
-- Answer: Int -> Int, error

-- • (\n -> n ‘div‘ 0) 5
-- Answer: Int, error

-- • j j
-- where
-- j x = j x
-- Answer: a -> a, indefinitely loops

-- • f (2, [1,2,3])
-- where
-- f (n, x:xs) = f (n-1, xs)
-- f (0, xs) = xs
-- Answer: (Int, [a]) -> [a], fails

-- • g 5
-- where
-- g 0 = 1
-- g n = 2 * g (n-2)

-- • d a, in the scope of the following declarations:
-- data T a = A [T a]
-- a = A [A [A [A [], A [A [A []]]], A [A [], A []]]]
-- m = foldr max 0
-- d (A []) = 1
-- d (A c) = 2 * m (map d c)

-- 3.2 Fill in the blanks
-- The following function nondec determines if the elements of a list appear in nondecreasing
-- order. Complete the definition, i.e., provide the missing right-hand side of the definition of f.
-- Do not change anything else.
nondec :: Ord alpha => [alpha] -> Bool
nondec xs = f (zip xs (tail xs))
    where 
        f = foldr (\(x, y) acc -> x <= y && acc) True

-- 3.3 Simple functions on numbers
-- The Goldbach conjecture states that any even number greater than two can be written as the
-- sum of two prime numbers. Using list comprehensions, write a function
-- goldbach :: Int -> [(Int,Int)]
-- which, when given an even number n, returns a list of all pairs of primes which sum to n. Note:
-- You will have to write a function which tests an integer for primality and this should be written
-- as a list comprehension also. For example, goldbach 6 should evaluate to [(3,3)]. When the
-- two primes in the pair are unequal, report them only once, smaller prime first. Report the pairs
-- in lexicographically sorted order. Thus, goldbach 20 should evaluate to [(3,17),(7,13)].
goldbach :: Int -> [(Int, Int)]
goldbach n = [(x, y) | x <- primes, y <- primes, x + y == n, x <= y]
    where
        primes = [x | x <- [2..n], isPrime x]
        isPrime x = null [y | y <- [2..x-1], x `mod` y == 0]

-- 3.4 Graphs
-- Many representations for graphs are possible, but here we use a very simple one. We represent
-- a directed graph as a set of edges, and an edge as a pair of numbers which are the vertex labels:
type Graph = [(Int, Int)]
-- • Are all directed graphs representable in this fashion?
-- Answer: Yes
exampleGraph :: Graph
exampleGraph = [(1, 2), (1, 3), (3, 4)]

-- | Get the unique vertices in the graph
vertices :: Graph -> [Int]
vertices g = unique $ concatMap (\(x, y) -> [x, y]) g

-- | Helper function to get unique elements in a list (like nub)
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
  | x `elem` xs = unique xs
  | otherwise   = x : unique xs

-- | Depth-First Search (DFS) to collect all reachable nodes
dfsReachable :: Graph -> Int -> [Int] -> [Int]
dfsReachable g v visited
  | v `elem` visited = visited
  | otherwise        = foldl (\acc u -> dfsReachable g u acc) (v : visited) neighbors
  where
    neighbors = [y | (x, y) <- g, x == v] ++ [x | (y, x) <- g, y == v]

-- | Check if the graph is connected
isConnected :: Graph -> Bool
isConnected g
  | null g    = False  -- An empty graph is not connected
  | otherwise = length reachableNodes == length allNodes
  where
    allNodes = vertices g
    reachableNodes = dfsReachable g (head allNodes) []

-- | Depth-First Search (DFS) with cycle detection
dfsCycle :: Graph -> Int -> Int -> [Int] -> Bool
dfsCycle g parent v visited
  = any (\u -> if u == parent then False
               else if u `elem` visited then True
               else dfsCycle g v u (v : visited)) neighbors
  where
    neighbors = [y | (x, y) <- g, x == v] ++ [x | (y, x) <- g, y == v]

-- | Check if the graph has a cycle
hasCycle :: Graph -> Bool
hasCycle g = any (\v -> dfsCycle g (-1) v []) (vertices g)


-- | Count the number of edges in the graph
numEdges :: Graph -> Int
numEdges g = length g

-- | Count the number of unique vertices in the graph
numVertices :: Graph -> Int
numVertices g = length (vertices g)

-- | Check if a graph is a tree
isTree :: Graph -> Bool
isTree g
  | null g        = False  -- An empty graph isn't a tree
  | otherwise     = (isConnected g) && not (hasCycle g) && (numEdges g == numVertices g - 1)

-- • Write a function isDAG :: Graph -> Bool with the obvious meaning.
-- isDAG :: Graph -> Bool

-- • A rooted dag is a dag with a unique distinguished vertex (the root) from which all ver-
-- tices are reachable. Write a function isRootedDAG :: Graph -> Bool with the obvious
-- meaning.
-- isRootedDAG :: Graph -> Bool


-- • Assuming that the given graph is a rooted dag, write a function
-- calcDepth :: Graph -> [(Int, Int)] to produce a list of pairs of the form (vertex,
-- depth), with one pair for each vertex in the graph, where the depth is the length of the
-- shortest path from the root to the vertex
-- calcDepth :: Graph -> [(Int, Int)]

-- 3.5 Paths in trees
-- We can define binary trees without any interesting content as follows:
data T = Leaf 
       | Node T T

-- A path from the root to any subtree consists of a series of instructions to go left or right, which
-- can be represented using another datatype:
data P = GoLeft P 
       | GoRight P 
       | This
       deriving Show

-- where the path This denotes the whole tree. Given some tree, we would like to find all paths,
-- i.e., the list of all paths from the root of the given tree to each of its subtrees. Write a function
-- allpaths :: T -> [P] to do so.
-- For instance, allpaths (Node Leaf (Node Leaf Leaf)) should evaluate to
-- [This,GoLeft This,GoRight This,GoRight (GoLeft This),GoRight (GoRight This)]
-- (but the ordering of the paths is immaterial).
allpaths :: T -> [P]
allpaths Leaf = [This]
allpaths (Node l r) = This : (map GoLeft (allpaths l) ++ map GoRight (allpaths r))

-- 3.6 Renumbering trees
-- Given a type of labelled binary trees
data Tree a = E
            | T a (Tree a) (Tree a)
-- write a function bfnum :: Tree a -> Tree Int to create a tree of the same shape as the input
-- tree, but with the values of the nodes replaced with the numbers 1 . . . N in breadth-first order,
-- where N is the number of internal T nodes in the input tree. Aim for correctness, simplicity,
-- elegance, and, preferably, algorithmic efficiency.
-- Example:
-- bfnum (T ’a’ (T ’b’ E (T ’c’ E E)) (T ’d’ E E))
-- evaluates to
-- T 1 (T 2 E (T 4 E E)) (T 3 E E)

-- bfnum :: Tree a -> Tree Int
-- bfnum t = fst (bfnum' t 1)
--     where
--         bfnum' E n = (E, n)
--         bfnum' (T _ l r) n = (T n l' r', n'')
--             where
--                 (l', n') = bfnum' l (n + 1)
--                 (r', n'') = bfnum' r n'