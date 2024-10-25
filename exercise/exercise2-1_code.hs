-- ************************************************************************* --
-- 2.1 Typing: Write the type of the following expressions

-- [’a’,’b’] :: [Char]
-- (’a’,’b’) :: (Char, Char)
-- [tail, reverse] :: [[a] -> [a]]
-- [tail [’a’], reverse [’a’]] :: [[Char]]
-- [tail [], reverse [’a’,’b’]] :: [[Char]]


-- ************************************************************************* --
-- 2.2 Function Definition and Types: Given the following equations provide 
-- a correct type for each expression
-- second xs = head (tail xs) 
-- swap (x, y) = (y, x)
-- double x = x * 2
-- palindrome xs = reverse xs == xs
-- twice f x = f (f x)
-- flip f x y = f y x
-- snoc x xs = xs ++ [x]

-- second ["a", "b", "c"] :: [Char]
-- second :: [a] -> a 
-- (second [tail, reverse]) [’a’, ’b’] :: [Char]
-- swap :: (a, b) -> (b, a)
-- snoc :: a -> [a] -> [a]
-- double :: Num a => a -> a
-- twice :: (t -> t) -> t -> t
-- flip :: (t1 -> t2 -> t) -> t2 -> t1 -> t
-- palindrome :: [a] -> Bool
-- twice double :: Num a => a -> a
-- twice swap ("a", "b") :: ([Char], [Char])


-- ************************************************************************* --
-- 2.3 Function definitions and evaluation: Given the same equations as 
-- above, evaluate:

-- flip zip ["a", "b"] ["c", "d"] 
--      [("b","d"), ("a","c")]
-- twice double 10
--      40
-- map palindrome (map (twice reverse) ["a", "bc"])
--      [True, False]

-- Small programs
-- General advice: Most of these questions involve lists and call for recursive functions over
-- lists. Try to write a direct recursive definition first. Then try to express the function in
-- terms of higher-order list functions (map, filter, foldr, etc.). There is often more than one
-- way to do so. Explore different solutions. Take this opportunity to consult the Haskell
-- library documentation (e.g., for prefixSum). Finally, for some problems one can find
-- elegant solutions in terms of list comprehensions.

-- 2.4 Matrices: In this exercise, we adopt the type [Double] as our representation of column vectors and the
-- type [[Double]] as our representation of matrices as lists of column vectors, and we develop
-- functions for matrix arithmetic. Preface your code with the declaration:
type Realvector = [Double]
type Realmatrix = [[Double]]
m1 = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]]
v1 = [1.0, 2.0, 3.0]

-- You may assume that the supplied vectors and matrices are compatible and non-singular, as
-- needed.

-- 2.4.1. Write a function rmvp :: Realmatrix -> Realvector -> Realvector that computes a
-- matrix-vector product.
rmvp :: Realmatrix -> Realvector -> Realvector
rmvp [] _ = []
rmvp mat vec 
    | all null mat = []
    | otherwise = foldr (+) 0 (zipWith (*) (map head mat) vec) : rmvp (map tail mat) vec

-- 2.4.2. Write a function rmmp :: Realmatrix -> Realmatrix -> Realmatrix that computes a
-- matrix-matrix product.
rmmp :: Realmatrix -> Realmatrix -> Realmatrix
rmmp [] _ = []
rmmp mat1 mat2
    | all null mat2 = []
    | otherwise = rmvp mat1 (head mat2) : rmmp mat1 (tail mat2)

-- 2.4.3. Write a function rmt :: Realmatrix -> Realmatrix to compute the matrix transpose.
rmt :: Realmatrix -> Realmatrix
rmt [] = []
rmt mat
    | all null mat = [] 
    | otherwise = map head mat : rmt (map tail mat)

-- 2.4.4. Write a function rminv :: Realmatrix -> Realmatrix to compute the matrix inverse.
rminv :: Realmatrix -> Realmatrix
rminv [] = []
rminv mat
    | all null mat = [] 
    | otherwise = mat

-- 2.5 Simple functions on numbers:
-- Define a function, collatz :: [Int] -> Int, that takes in a list of starting numbers and re-
-- turns the one which gives rise to the longest Collatz sequence. For example, collatz [1..20]
-- should evaluate to 19.
-- If multiple starting numbers have the same sequence length, your function should return the
-- largest of them. For example, 18 and 19 both produce a sequence length of 21, and so 19 is
-- reported.
-- NB. Once the sequence starts, the terms can become quite large. Your code must be prepared
-- to handle this possibility.
collatz :: [Int] -> Int
collatz [] = 0
collatz xs = firstOfMaxSecond (reverse (zip xs (map (length . collatzSeq) xs)))
    where
        collatzSeq :: Int -> [Int]
        collatzSeq 1 = [1]
        collatzSeq n 
            | even n = n : collatzSeq (n `div` 2)
            | odd n = n : collatzSeq (3 * n + 1)
        firstOfMaxSecond :: [(Int, Int)] -> Int
        firstOfMaxSecond = fst . foldl1 (\acc x -> if snd x > snd acc then x else acc)

-- 2.6 Simple functions on lists
-- Write a function select :: (t -> Bool) -> [t] -> [a] -> [a], which takes a predicate
-- and two lists as arguments and returns a list composed of elements from the second list in those
-- positions where the predicate holds when applied to the element in the corresponding position
-- of the first list. For example, select even [1..26] "abcdefghijklmnopqrstuvwxyz" evalu-
-- ates to "bdfhjlnprtvxz".
select :: (t -> Bool) -> [t] -> [a] -> [a]
select _ [] _ = []
select _ _ [] = []
select p (x:xs) (y:ys) 
    | p x = y : select p xs ys
    | otherwise = select p xs ys

-- 2.7 Simple functions on lists and numbers
-- Write a function prefixSum :: [Int] -> [Int], which takes a list of numbers as its argument
-- and returns a list of sums of all prefixes of the list. For example, prefixSum [1..10] evaluates
-- to [1,3,6,10,15,21,28,36,45,55].
prefixSum :: [Int] -> [Int]
prefixSum [] = []
prefixSum (x:xs) = x : map (+x) (prefixSum xs)

-- 2.8 Simple functions on lists and numbers
-- Write a function numbers :: [Int] -> Int, which takes a list of integers (each of them be-
-- tween zero and nine) as its argument and returns the integer which has those numbers as
-- digits in the usual decimal notation. For example, numbers [1..4] evaluates to 1234.
numbers :: [Int] -> Int
numbers [] = 0
numbers xs = foldl (\acc x -> (acc * 10) + x) 0 xs

-- 2.9 Using lists for arithmetic: writing recursive functions over lists
-- Numerals can be represented as lists of integers. For instance, decimal 
-- numerals can be expressed as lists of integers from 0 to 9. The 
-- integer 12345678901234567890 might be represented as the Haskell 
-- list [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0] :: [Int].
-- However, the representation should allow a radix (base) other than 10 as well.
-- We use the following type abbreviation:
type Numeral = (Int, [Int])
-- where the first component of the pair is the radix and the second is the list of digits.
-- The above example number is then represented as:
-- example :: Numeral
-- example = (10, [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0])

-- 2.9.1. makeLongInt :: Integer -> Int -> Numeral, such that makeLongInt n r computes the
-- list representation of the integer n in radix r. You can assume that n ≥ 0, and that r >1.
-- For example, makeLongInt 123 10 should evaluate to (10, [1,2,3])
makeLongInt :: Integer -> Int -> Numeral
makeLongInt 0 r = (r, [0])
makeLongInt n r = (r, reverse(makeLongInt' n r))
    where
        makeLongInt' :: Integer -> Int -> [Int]
        makeLongInt' 0 _ = []
        makeLongInt' n r = fromIntegral(n `mod` (toInteger r)) : makeLongInt' (n `div` (toInteger r)) r

-- 2.9.2. evaluateLongInt :: Numeral -> Integer, such that evaluateLongInt (r, l) converts
-- a numeral back to a Haskell integer. You can assume that l is a valid list for radix r. For
-- example, evaluateLongInt (10, [1,2,3]) should evaluate to 123
evaluateLongInt :: Numeral -> Integer
evaluateLongInt (_, []) = 0
evaluateLongInt (r, (x:xs)) = toInteger(x * (r ^ (length xs))) + evaluateLongInt (r, xs)

-- 2.9.3. changeRadixLongInt :: Numeral -> Int -> Numeral, such that changeRadixLongInt
-- n r computes the representation of the same number as n in a new radix r. For example,
-- changeRadixLongInt (10, [1,2,3]) 8 should evaluate to (8, [1,7,3]); on the other
-- hand, changeRadixLongInt (10, [1,2,3]) 16 should evaluate to (16, [7,11]).
-- The computation should be carried out without the use of Haskell’s built-in Integer
-- arithmetic. In particular, the following implementation must be understood only as a
-- specification (because it uses Integer arithmetic within the functions makeLongInt and
-- evaluateLongInt):
-- changeRadixLongInt (r1, ds1) r2 = makeLongInt (evaluateLongInt (r1, ds1)) r2
-- Additional examples: changeRadixLongInt (16, [13,14,10,13,11,14,14,15]) 17 eval-
-- uates to (17, [9,1,13,3,6,16,7,8]).
changeRadixLongInt :: Numeral -> Int -> Numeral
changeRadixLongInt (r1, ds1) r2 = makeLongInt (evaluateLongInt (r1, ds1)) r2


-- 2.9.4. addLongInts :: Numeral -> Numeral -> Numeral, such that addLongInts a b computes
-- the sum of the numbers given by the numerals a and b. If a and b use the same radix, that
-- radix should be used for the result. If a and b use different radices, the result should use
-- the larger one. For example, addLongInts (10, [1,2,3]) (3, [1]) should evaluate to
-- (10, [1,2,4]).
-- The computation should be carried out without the use of Haskell’s built-in Integer
-- arithmetic. In particular, the following implementation must be understood only as a
-- specification (because it uses Integer arithmetic in (+) as well as within the functions
-- makeLongInt and evaluateLongInt):
-- CS 558 Software Foundations, Fall 2024 5
-- addLongInts (r1, ds1) (r2, ds2)
-- | r1 == r2 = makeLongInt (evaluateLongInt (r1, ds1) + evaluateLongInt (r2, ds2)) r1
-- | r1 < r2 = addLongInts (changeRadixLongInt (r1, ds1) r2) (r2, ds2)
-- | r1 > r2 = addLongInts (r1, ds1) (changeRadixLongInt (r2, ds2) r1)
-- It is not permissible to implement the addition of a and b as b +∑a1 1 (repeated Succ).
-- Additional examples: addLongInts (16, [13,14,10,13,11,14,14,15]) (8, [7, 7, 7])
-- evaluates to (16, [13,14,10,13,12,0,14,14])
addLongInts :: Numeral -> Numeral -> Numeral
addLongInts (r1, ds1) (r2, ds2)
    | r1 == r2 = makeLongInt (evaluateLongInt (r1, ds1) + evaluateLongInt (r2, ds2)) r1
    | r1 < r2 = addLongInts (changeRadixLongInt (r1, ds1) r2) (r2, ds2)
    | r1 > r2 = addLongInts (r1, ds1) (changeRadixLongInt (r2, ds2) r1)


-- 2.9.5. mulLongInts :: Numeral -> Numeral -> Numeral, such that mulLongInts a b computes
-- the product of the numbers given by the numerals a and b. If a and b use the same radix,
-- that radix should be used for the result. If a and b use different radices, the result should
-- use the larger one. For example, mulLongInts (10, [1,2,3]) (3, [1]) should evalu-
-- ate to (10, [1,2,3]).
-- The computation should be carried out without the use of Haskell’s built-in Integer
-- arithmetic. In particular, the following implementation must be understood only as a
-- specification (because it uses Integer arithmetic in (*) as well as within the functions
-- makeLongInt and evaluateLongInt):
-- mulLongInts (r1, ds1) (r2, ds2)
-- | r1 == r2 = makeLongInt (evaluateLongInt (r1, ds1) * evaluateLongInt (r2, ds2)) r1
-- | r1 < r2 = mulLongInts (changeRadixLongInt (r1, ds1) r2) (r2, ds2)
-- | r1 > r2 = mulLongInts (r1, ds1) (changeRadixLongInt (r2, ds2) r1)
-- It is not permissible to implement the multiplication of a and b as ∑a1 b (repeated addition).
-- Additional examples: mulLongInts (16, [13,14,10,13,11,14,14,15]) (8, [7, 7, 7])
-- evaluates to (16, [1,11,12,7,12,13,0,1,15,1,1]).
mulLongInts :: Numeral -> Numeral -> Numeral
mulLongInts (r1, ds1) (r2, ds2)
    | r1 == r2 = makeLongInt (evaluateLongInt (r1, ds1) * evaluateLongInt (r2, ds2)) r1
    | r1 < r2 = mulLongInts (changeRadixLongInt (r1, ds1) r2) (r2, ds2)
    | r1 > r2 = mulLongInts (r1, ds1) (changeRadixLongInt (r2, ds2) r1)