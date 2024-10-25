-- Shape is not polymorphic and not recursive
data Shape = Circle Double
              | Rectangle Double Double
              | Triangle Double Double Double
                -- deriving (Eq, Ord)
-- Circle, rectangle, and triangle are value constructors

shapes :: [Shape]
shapes = [Circle 2.0, Rectangle 3.0 4.0, Triangle 5.0 12.0 13.0]

circles :: [Shape]
circles = map Circle [1.0, 2.0, 3.0]

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle a b c) = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

-- Cannot use deriving if we want to define our own instance of Show
instance Show Shape where
    show (Circle r) = "Circle with radius " ++ show r
    show (Rectangle w h) = "Rectangle with width " ++ show w ++ " and height " ++ show h
    show (Triangle a b c) = "Triangle with sides " ++ show a ++ ", " ++ show b ++ ", and " ++ show c

instance Eq Shape where
    (Circle r1) == (Circle r2) = r1 == r2
    (Rectangle w1 h1) == (Rectangle w2 h2) = w1 == w2 && h1 == h2
    (Triangle a1 b1 c1) == (Triangle a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2
    _ == _ = False

-- Polymorphic, but not recursive
data Maybe a = Nothing | Just a
-- a is our type parameter

-- Examples of types:
-- Nothing :: Maybe a
-- Just :: a -> Maybe a
-- Just 3 :: Maybe Int
-- Just ["a","b"] :: Maybe [String]

-- Polymorphic, but not recursive
data Either a b = Left a | Right b

-- Recursive, but not polymorphic
data Nat = Zero | Succ Nat

one :: Nat
one = Succ Zero
two :: Nat
two = Succ one

-- Succ (Succ (Succ Zero)) == Nat

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = add m (Succ n)

instance Eq Nat where
    Succ x == Succ y = x == y
    Zero == Zero = True
    Succ x == Zero = False
    Zero == Succ _ = False