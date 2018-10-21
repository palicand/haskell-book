module Ch12.Natural where
    data Nat = Zero | Succ Nat deriving (Eq, Show)

    natToInteger :: Nat -> Integer
    natToInteger Zero     = 0
    natToInteger (Succ a) = 1 + natToInteger a

    integerToNat' :: Integer -> Nat
    integerToNat' 0   = Zero
    integerToNat' int = Succ $ integerToNat' $ int - 1

    integerToNat :: Integer -> Maybe Nat
    integerToNat int
        | int >= 0  = Just $ integerToNat' int
        | otherwise = Nothing
