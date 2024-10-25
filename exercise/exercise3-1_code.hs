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

-- • (Consider the graph as undirected for this question.) Write a function
-- isTree :: Graph -> Bool with the obvious meaning.
isTree :: Graph -> Bool
isTree g = isConnectedGraph g && not (hasCycle g)

getVertices :: Graph -> [Int]
getVertices g = [x | (x,y) <- g, x `mod` 2 == 0, x `mod` 3 == 0]

isConnectedGraph :: Graph -> Bool
isConnectedGraph g =

hasCycle :: Graph -> Bool


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
