-- ************************************************************************* --
-- 5.1 Files
-- Write standalone Haskell programs that work like the Unix cat, tac, rev, and sort commands
-- (with no comamand-line options).

-- ************************************************************************* --
-- 5.2 Booleans and numbers
data Term = Tru
          | Fls
          | If Term Term Term
          | Zero
          | Succ Term
          | Pred Term
          | IsZero Term
          deriving Eq

-- (Complete the Show instance so terms will be printed to match the concrete syntax.)
instance Show Term where
  show Tru = "true"
  show Fls = "false"
  show Zero = "0"
  show (Succ t) = "succ (" ++ show t ++ ")"
  show (Pred t) = "pred (" ++ show t ++ ")"
  show (IsZero t) = "iszero (" ++ show t ++ ")"
  show (If t1 t2 t3) = "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3 ++ " fi"

data Token = TokenTrue
          | TokenFalse
          | TokenIf
          | TokenThen
          | TokenElse
          | TokenFi
          | TokenSucc
          | TokenPred
          | TokenIsZero
          | TokenZero
          | TokenLPar
          | TokenRPar
          deriving (Eq, Show)

-- (1) Write a scanner function scan :: String -> Maybe [Token]. White space is not required
-- around parentheses.
-- Improved scan function with whitespace and additional tokens
scan :: String -> Maybe [Token]
scan [] = Just []
scan (' ':input) = scan input -- Skip whitespace
scan input
    | take 4 input == "true"   = fmap (TokenTrue :) (scan (drop 4 input))
    | take 5 input == "false"  = fmap (TokenFalse :) (scan (drop 5 input))
    | take 2 input == "if"     = fmap (TokenIf :) (scan (drop 2 input))
    | take 4 input == "then"   = fmap (TokenThen :) (scan (drop 4 input))
    | take 4 input == "else"   = fmap (TokenElse :) (scan (drop 4 input))
    | take 2 input == "fi"     = fmap (TokenFi :) (scan (drop 2 input))
    | take 4 input == "succ"   = fmap (TokenSucc :) (scan (drop 4 input))
    | take 4 input == "pred"   = fmap (TokenPred :) (scan (drop 4 input))
    | take 6 input == "iszero" = fmap (TokenIsZero :) (scan (drop 6 input))
    | take 1 input == "0"      = fmap (TokenZero :) (scan (drop 1 input))
    | take 1 input == "("      = fmap (TokenLPar :) (scan (drop 1 input))
    | take 1 input == ")"      = fmap (TokenRPar :) (scan (drop 1 input))
    | otherwise                = Nothing -- Unrecognized token


-- (2) Write a parser function parse :: [Token] -> Maybe Expr.
-- Parse function using descriptive names for functions
parse :: [Token] -> Maybe Term
parse tokens = case tokens of
  [TokenTrue] -> Just Tru
  [TokenFalse] -> Just Fls
  [TokenZero] -> Just Zero
  TokenSucc : rest -> fmap Succ (parse rest)
  TokenPred : rest -> fmap Pred (parse rest)
  TokenIsZero : rest -> fmap IsZero (parse rest)
  TokenIf : cond : TokenThen : t1 : TokenElse : t2 : TokenFi : rest ->
    fmap3 If (parse [cond]) (parse [t1]) (parse [t2])
  _ -> Nothing

-- Helper function to apply a function to three arguments within the Maybe monad
fmap3 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
fmap3 f (Just x) (Just y) (Just z) = Just (f x y z)
fmap3 _ _ _ _ = Nothing


-- (3) Write a main function that reads in a source file in the language of booleans and natural
-- numbers, scans it, parses it, and prints out an abstract syntax tree (an Expr) for syntactically
-- valid inputs, or otherwise prints a suitable error message.