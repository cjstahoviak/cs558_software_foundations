data Tree a = Leaf a | Fork [Tree a]

dfs :: Tree a -> [a]
dfs (Leaf l) = [l]
dfs (Fork []) = []
dfs (Fork ts) = concat (map dfs ts)