-- ************************************************************************* --
-- 6.1 Booleans and numbers
data Term = Tru
          | Fls
          | If Term Term Term
          | Zero
          | Succ Term
          | Pred Term
          | IsZero Term
          deriving (Eq, Show)

isValue :: Term -> Bool
isValue Tru = True
isValue Fls = True
isValue t = isNumericValue t

isNumericValue :: Term -> Bool
isNumericValue Zero = True
isNumericValue (Succ t) = isNumericValue t
isNumericValue _ = False

eval1 :: Term -> Maybe Term -- the one-step evaluation relation represented as a function
eval1 (If Tru t2 _) = Just t2
eval1 (If Fls _ t3) = Just t3
eval1 (If t1 t2 t3) = fmap (\t1' -> If t1' t2 t3) (eval1 t1)
eval1 (Succ t) = fmap Succ (eval1 t)
eval1 (Pred Zero) = Just Zero
eval1 (Pred (Succ t)) | isNumericValue t = Just t
eval1 (Pred t)                 = fmap Pred (eval1 t)
eval1 (IsZero Zero)            = Just Tru
eval1 (IsZero (Succ t)) | isNumericValue t = Just Fls
eval1 (IsZero t)               = fmap IsZero (eval1 t)
eval1 _                        = Nothing

-- eval :: Term -> Term -- multi-step reduction (iterates eval1 as many times as possible)
eval :: Term -> Term
eval t 
    | Just t' <- eval1 t = eval t'
    | otherwise = t