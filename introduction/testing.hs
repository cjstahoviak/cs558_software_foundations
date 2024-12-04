data Tree a = Leaf a 
            | Fork [Tree a]

dfs :: Tree a -> [a]
dfs (Leaf l) = [l]
dfs (Fork []) = []
dfs (Fork (t:ts)) = dfs t ++ dfs (Fork ts)

-- dfs [Leaf 1, Fork [Leaf 2, Leaf 3, Fork [Leaf 4, Leaf 5]]]