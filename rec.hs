module Recursion where

recSum :: (Eq a, Num a) => a -> a
recSum 0 = 0
recSum n = n + recSum (n-1)

multRecSum :: (Integral a) => a -> a -> a
multRecSum n 0 = 0
multRecSum n m = n + multRecSum n (m - 1)

dividedBy :: Integral a => a -> a -> Maybe (a, a)
dividedBy num 0 = Nothing
dividedBy num denom = Just $ go num denom 0
    where
        go n   d count
           | abs(n) < abs(d) && sn == sd = (count, n)
           | sn * sd == 1  = go (n - d) d (count + 1)
           | otherwise     = go (n + d) d (count - 1)
         where
           sn = signum n
           sd = signum d

mc91 :: Integer -> Integer
mc91 n
    | n > 100   = n - 10
    | otherwise = mc91(mc91(n + 11))
