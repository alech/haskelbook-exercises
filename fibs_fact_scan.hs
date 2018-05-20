module Scan where

fibs20 :: [Integer]
fibs20 = take 20 $ 1 : scanl (+) 1 fibs20

fibsUnder100 :: [Integer]
fibsUnder100 = takeWhile (<100) $ 1 : scanl (+) 1 fibsUnder100

factorial :: Int -> Integer
factorial n =
    (scanl (*) 1 [1..]) !! n
