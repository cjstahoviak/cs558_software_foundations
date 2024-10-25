
tuple1 = (2, "abc")

x1 = fst tuple1

x2 = snd tuple1

tuple2 :: (Int, String, Double)
tuple2 = (3, "a", 3.1)

(x3, x4, x5) = tuple2

x6 = zip [1,2,3,4] ['a','b','c','d']

x7 = zip [1,2,3,4] ['a','b','c','d','e']

x8 = unzip x6

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' (pair:pairs) =
  let
    (x,y) = pair
    (xs,ys) = unzip' pairs
  in
    (x:xs,y:ys)

unzip'' :: [(a,b)] -> ([a],[b])
unzip'' [] = ([],[])
unzip'' (pair:pairs) = (x:xs,y:ys)
  where
    (x,y) = pair
    (xs,ys) = unzip'' pairs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

ones :: [Int]
ones = 1 : ones
