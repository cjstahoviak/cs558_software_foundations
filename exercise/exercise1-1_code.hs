-- ************************************************************************* --
-- 1.1 Typing: Write the type of the following expressions

-- (True,True:[]) :: (Bool,[Bool])
-- \x -> \y -> (x && y, y || x) :: Bool -> Bool -> (Bool,Bool)
-- "a"++"b" :: [Char]
-- map tail ["a","ab","abc"] :: [[Char]]
-- zip :: [(a,b)]
-- zip ["a","ab","abc"] ["bcd","cd","d"] :: [([Char],[Char])]
-- map zip :: [(a,b)]
-- map (uncurry (++)) (zip ["a","ab","abc"] ["bcd","cd","d"]) :: [[Char]]


-- ************************************************************************* --
-- 1.2 Lists and Functions: Which of the statements are true for all finite lists xs

-- reverse (map f xs) = map f (reverse xs)  ==> True
-- map f (map g xs) = map g (map f xs)      ==> False
-- reverse (reverse xs) = reverse xs        ==> False
-- map f (map f xs) = map f xs              ==> False
-- reverse xs = xs                          ==> False


-- ************************************************************************* --
-- 1.3 Simple Functions: Write a Haskell function test :: Int -> Int -> Bool that 
-- takes two integers and returns True if and only if the two integers are both odd.

test :: Int -> Int -> Bool
test a b = odd a && odd b


-- ************************************************************************* --
-- 1.4 List Manipulation: Write a Haskell function duplicate :: [Char] -> [Char] 
-- that takes a list of elements andreturns a list where every element has been 
-- duplicated

duplicate :: [Char] -> [Char]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs


-- ************************************************************************* --
-- 1.5 List Manipulation: Write a Haskell function compress :: [Char] -> [Char] 
-- that eliminates consecutive duplicate elements of a list. For example, compress 
-- "HHeelllloo WWoorrlldd" evaluates to "Helo World".

compress :: [Char] -> [Char]
compress [] = []
compress [x] = [x]
compress (x:xs)  
    | x == (head xs) = compress xs
    | otherwise = x : compress xs


-- ************************************************************************* --
-- 1.6 List Manipulation: Write a Haskell function zipSum :: [Int] -> [Int] -> [Int] 
-- that takes two equally sized lists of ints and returns a single list of ints in 
-- which each element at a given index is the sum of the corresponding values at that 
-- index from the input lists.

zipSum :: [Int] -> [Int] -> [Int]
zipSum [] [] = []
zipSum (x:xs) (y:ys) = (x + y) : zipSum xs ys
zipSum _ _ = error "Lists must be of equal size!"

-- ************************************************************************* --
-- 1.7 Using Lists for Sets: Let us use the Haskell type [Integer] to represent 
-- sets of integers. The representation invariants are that there are no duplicates
-- in the list, and that the order of the list elements is increasing. Do not use 
-- any of the built-in Haskell functions that manipulate lists as sets. Do not use
-- any Haskell libraries for sets.

-- 1.7.1. Write a Haskell function setUnion: [Integer] -> [Integer] -> [Integer] that takes
-- two sets and returns their union.

-- Couldn't figure out how to do this without a helper function
memberSet :: Integer -> [Integer] -> Bool 
memberSet x [] = False 
memberSet x (y:ys)
  | x == y = True 
  | otherwise = memberSet x ys

setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion [] [] = []
setUnion (x:xs) [] = (x:xs)
setUnion [] (y:ys) = (y:ys)
setUnion (x:xs) (y:ys)
  | memberSet y (x:xs) = setUnion (x:xs) ys
  | otherwise = y : setUnion (x:xs) ys

setUnion' :: [Integer] -> [Integer] -> [Integer]
setUnion' xs ys = [x | x <- xs] ++ [y | y <- ys, not (memberSet y xs)]

-- 1.7.2. Write a Haskell function setIntersection: [Integer] -> [Integer] -> [Integer] that
-- takes two sets and returns their intersection.
setIntersection :: [Integer] -> [Integer] -> [Integer]
setIntersection [] [] = []
setIntersection (x:xs) [] = []
setIntersection [] (y:ys) = []
setIntersection (x:xs) (y:ys)
  | memberSet y (x:xs) = y : setIntersection (x:xs) ys
  | otherwise = setIntersection (x:xs) ys

setIntersection' :: [Integer] -> [Integer] -> [Integer]
setIntersection' xs ys = [x | x <- xs, memberSet x ys]

-- 1.7.3. Write a Haskell function setDifference: [Integer] -> [Integer] -> [Integer] that
-- takes two sets and returns their set difference.
setDifference :: [Integer] -> [Integer] -> [Integer]
setDifference [] [] = []
setDifference (x:xs) [] = (x:xs)
setDifference [] (y:ys) = []
setDifference (x:xs) (y:ys)
  | memberSet x (y:ys) = setDifference xs (y:ys)
  | otherwise = x : setDifference xs (y:ys)

setDifference' :: [Integer] -> [Integer] -> [Integer]
setDifference' xs ys = [x | x <- xs, not (memberSet x ys)]

-- 1.7.4. Write a Haskell function setEqual: [Integer] -> [Integer] -> Bool that takes two
-- sets and returns True if and only if the two sets are equal.

-- Helper function to remove all element from a list
removeAll :: Integer -> [Integer] -> [Integer]
removeAll _ [] = [] 
removeAll x (y:ys)
  | x == y = removeAll x ys
  | otherwise = y : removeAll x ys

setEqual :: [Integer] -> [Integer] -> Bool
setEqual [] [] = True
setEqual [] _ = False
setEqual _ [] = False
setEqual (x:xs) (y:ys)
  | memberSet x (y:ys) = setEqual xs (removeAll x (y:ys))
  | otherwise = False

-- 1.7.5. Write a Haskell function powerSet: [Integer] -> [[Integer]] that takes a set Sand
-- returns its powerset 2^S. (The powerset 2^S of a set S(sometimes written P(S)) is the set of
-- all subsets of S.) Note that the result uses the type [[Integer]] to represent sets of sets
-- of integers. Here the representation invariant is that there are no duplicates in the list; the
-- order of the sublists is immaterial.
powerSet :: [Integer] -> [[Integer]]
powerSet [] = [[]]
powerSet (x:xs) = [x:ps | ps <- powerSet xs] ++ powerSet xs

-- ************************************************************************* --
-- 1.8 Numbers

-- 1.8.1: Write a function mp :: Int -> [Int] such that mp n returns a list consisting of 
-- the first n Mersenne primes. Test this function for n up to 8.
mp :: Int -> [Int]
mp n = take n [2^p - 1 | p <- [2..], null [x | x <- [2..(floor (sqrt (fromIntegral (2^p - 1))))], (2^p - 1) `mod` x == 0]]

-- 1.8.2: What do you need to do in order to compute a few more Mersenne primes, for instance
-- up to 11?
