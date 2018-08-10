module Cipher where

import Data.Char

caesar :: Int -> Char -> Char
caesar i c
    | c `elem` enumFromTo 'a' 'z' = chr (ord 'a' + ((ord c - ord 'a' + i) `mod` 26))
    | c `elem` enumFromTo 'A' 'Z' = chr (ord 'A' + ((ord c - ord 'A' + i) `mod` 26))
    | otherwise                   = c -- do not "encrypt" nonalpha

unCaesar :: Int -> Char -> Char
unCaesar i = caesar (26 - i)

rot13 :: String -> String
rot13 = map (caesar 13)

vigenere :: String -> String -> String
vigenere keyword plain =
    zipWith (\a b -> caesar (charToShift a) b) (mconcat $ repeat keyword) plain
    where
        charToShift :: Char -> Int
        charToShift c
            | c `elem` enumFromTo 'a' 'z' = ord c - ord 'a'
            | c `elem` enumFromTo 'A' 'Z' = ord c - ord 'A'
            | otherwise                   = 0
