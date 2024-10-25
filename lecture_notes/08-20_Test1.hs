x1 = 3 + 4
x2 = 3 * 4
x3 = x1 `div` x2
newVariable = "a string" ++ (show x2)

-- let's write a function that takes a number and tells us how many digits there are (in the decimal representation)
countDigits :: Int -> Int
countDigits n = length (show n)

countDigits' n = if n < 0 then length (show (- n)) else length (show n)

x4 = True && False

countDigits'' :: Int -> Int
countDigits'' n
  | n < 0   = length (show (-n))
  | otherwise = length (show n)

  
