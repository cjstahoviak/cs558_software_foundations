-- ************************************************************************* --
-- 4.1 Program Equivalence
-- Prove that: map f . concat = concat . map (map f)

-- I.H:
-- map f (concat xs) = concat (map (map f) xs)

-- B.C: 
-- map f (concat []) = concat (map (map f) [])
-- map f [] = concat (map (map f) [])
-- [] = concat (map (map f) [])
-- [] = concat []
-- [] = []

-- I.C 
-- map f (concat (x:xs)) = concat (map (map f) (x:xs))
-- map f (x ++ concat xs) = concat (map (map f) (x:xs)) by def concat
-- map f x ++ map f (concat xs) = concat (map (map f) (x:xs)) by def map
-- map f x ++ concat (map (map f) xs) = concat (map (map f) (x:xs)) by I.H.
-- map f x ++ concat (map (map f) xs) = concat((map f x) : map (map f) xs) by expanding map
-- map f x ++ concat (map (map f) xs) = map f x ++ concat(map (map f) xs) by def concat
-- EQUALITY

-- ************************************************************************* --
-- 4.2 Program Equivalence
-- Under what conditions do the following two list comprehensions deliver the same result?
-- [e | x <- xs, p x, y <- ys]
-- and
-- [e | x <- xs, y <- ys, p x]

-- When the predicate p is either True or False for all elements xs (?)
-- Actually they're always both the same, I can't find a counter example

-- ************************************************************************* --
-- 4.3 Lists
-- Define a function disjoint :: (Ord a) => [a] -> [a] -> Bool that takes two lists in as-
-- cending order and determines whether or not they have an element in common.
-- Returns true if there are no elemtents in common between the two lists.
disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint [] _ = True
disjoint _ [] = True
disjoint (x:xs) (y:ys)
    | x == y = False
    | x < y = disjoint xs (y:ys)
    | otherwise = disjoint (x:xs) ys

-- ************************************************************************* --
-- 4.4 Implementing merge sort for lists
-- Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted
-- lists to give a single sorted list.
-- Define halve :: [a] -> ([a],[a]) that splits a list into two halves whose lengths differ by
-- at most one.
-- Then define a function msort :: Ord a => [a] -> [a] to carry out the usual merge-sort al-
-- gorithm, by splitting a list in half, sorting the two halves, and then merging them; recursion
-- stops at the empty list or singleton list.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = (take halfSize xs, drop halfSize xs)
  where
    halfSize = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst (halve xs))) (msort (snd (halve xs)))

-- ************************************************************************* --
-- 4.5 Fancy function application
-- Define a function fancyApp :: [a->b] -> [a] -> [b] that applies the functions from its first
-- argument to the elements of its second argument in round-robin fashion, so, for example,
-- fancyApp [(+1),(*2),(+10)] [1,2,3,4,5,6,7] evaluates to [2,4,13,5,10,16,8].
fancyApp :: [a -> b] -> [a] -> [b]
fancyApp _ [] = []
fancyApp fs xs = zipWith (\f x -> f x) fs (take functionLength xs) ++ fancyApp fs (drop functionLength xs)
    where
        functionLength = length fs

-- ************************************************************************* --
-- 4.6 Evaluators
-- Given the type declaration:
data Expr = Val Int 
          | Add Expr Expr 
-- define a higher-order func- tion folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a such 
-- that folde f g replaces each Val (value) constructor in an expression by the function f 
-- and each Add constructor by the function g.
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n)     = f n                    -- Apply f to the integer n in Val
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)  -- Apply g to the results of recursively evaluating e1 and e2

-- Then use folde to define a function eval :: Expr -> Int that evaluates an expression to an
-- integer value, a function size :: Expr -> Int that calculates the number of values in an ex-
-- pression, a function ops :: Expr -> Int that calculates the number of arithmetic operations
-- in an expression, a function fringe :: Expr -> [Int] that collects the values in an expres-
-- sion into a list, a function exprMap :: (Int -> Int) -> Expr -> Expr that applies the given
-- function to each value in an expression.
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

ops :: Expr -> Int
ops = folde (const 0) (\x y -> 1 + x + y)

fringe :: Expr -> [Int] 
fringe = folde (\x -> [x]) (++)

exprMap :: (Int -> Int) -> Expr -> Expr
exprMap f = folde (Val . f) Add

-- Finally, define a function simplify :: Expr -> Expr that will replace every addition with its
-- result provided the result is small, specifically, less than 100.
simplify :: Expr -> Expr
simplify = folde Val simplifyAdd
  where
    simplifyAdd x y = if x + y < 100 then Val (x + y) else Add x y

-- ************************************************************************* --
-- 4.7 Drawing
exampleDrawing :: [[(Float, Float)]]
exampleDrawing = [[(100.0,100.0),(100.0,200.0),(200.0,100.0)],[(150.0,150.0),(150.0,200.0),(200.0,200.0),(200.0,150.0)]]

exampleShape :: [(Float, Float)]
exampleShape = [(100.0,100.0),(100.0,200.0),(200.0,100.0)]

makeCommand :: [[(Float,Float)]] -> String
makeCommand [] = "showpage\n%%EOF"
makeCommand xs = "%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: 100.0 100.0 200.0 200.0\n" ++ concatMap formatShape xs

-- Helper function
formatShape :: [(Float,Float)] -> String
formatShape [] = "closepath\nstroke\n"
formatShape (x:xs) = (show (fst x)) ++ " " ++ (show (snd x)) ++ " moveto\n" ++ formatShape xs
