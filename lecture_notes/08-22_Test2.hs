{-
Int - integers, machine dependent
Integer - arbitrary precision integers
Float
Double
Bool
Char
-}

nnn :: Integer
nnn = 46547875454545747447474434343

d1, d2 :: Double
d1 = 0.5
d2 = 1.36e-9

c :: Char
c = 'a'

x = 9.0 / 2.0

x' = 9 `div` 2

x'' = div 9 2

square :: Int -> Int
square n = n * n

cube :: Int -> Int
cube n = n * n * n

fourthpower :: Int -> Int
fourthpower n = square (square n)

fourthpower' :: Int -> Int
fourthpower' = square . square      -- here . means function composition

greaterThanZero :: Double -> Bool
greaterThanZero x = x > 0

b1 :: Bool
b1 = 2 > 3.5

{- doesn't work - need explicit conversion
compareIntAndDouble :: Int -> Double -> Bool
compareIntAndDouble n x = n > x
-}

isMoreThanHalfOf :: Int -> Int -> Bool
isMoreThanHalfOf n m = n > m `div` 2

-- twice :: (Int -> Int) -> Int -> Int
twice :: (Int -> Int) -> (Int -> Int)
twice f n = f (f n)

fourthpower'' :: Int -> Int
fourthpower'' n = twice square n

fourthpower''' :: Int -> Int
fourthpower''' = twice square

hasAnEvenSquare :: Int -> Bool
-- hasAnEvenSquare n = even (square n)
hasAnEvenSquare = even . square


-- compose :: (Int -> Int) -> (Int -> Int) -> Int -> Int
-- compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
