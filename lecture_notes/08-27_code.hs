fact :: Integer -> Integer
fact n = if n == 0 then 1 else n * fact (n - 1)

fact' :: Integer -> Integer
fact' 0 = 1
fact' n = n * fact' (n - 1) 

fact'' :: Integer -> Integer
fact'' n
    | n == 0 = 1
    | otherwise = n * fact'' (n - 1)

fact''' :: Integer -> Integer
fact''' n = product [1..n]

takey :: Int -> [a] -> [a]
takey 0 _ = []
takey _ [] = []
takey n (x:xs) = x : take (n - 1) xs