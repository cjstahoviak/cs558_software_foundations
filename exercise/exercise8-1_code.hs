-- 8.1 Booleans and numbers - typing
-- (Continued from exercises 5, 6, and 7.)
-- Implement a simple type checker, namely a Haskell function findType :: Term -> Maybe Typ,
-- where we have defined 
data Typ = TypNat | TypBool.
-- Then implement a version that produces a complete proof tree (according to the inference rules
-- in Figures 8-1 and 8-2 of the textbook).
x
findType :: Term -> Maybe Typ