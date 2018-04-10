module Eft where

eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool False False = [False]
eftBool True True = [True]
eftBool False True = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
    | a == b = [a]
    | a == LT && b == EQ = [a, b]
    | a == EQ && b == GT = [a, b]
    | a == LT && b == GT = [a, EQ, b]
    | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt a b
    | a <= b    = a : eftInt (succ a) b
    | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar a b
    | a <= b    = a : eftChar (succ a) b
    | otherwise = ""
