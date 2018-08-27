module Naturals where

data Nat =
      Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x < 0     = Nothing
    | otherwise = Just $ buildNat x
    where
        buildNat 0 = Zero
        buildNat n = Succ (buildNat (n - 1))
