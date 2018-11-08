module Main where

import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\a b -> a == x || b) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' ta
    | null ta   = Nothing
    | otherwise = Just $ foldr1 (\a b -> if b < a then b else a) ta

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' ta
    | null ta   = Nothing
    | otherwise = Just $ foldr1 (\a b -> if b > a then b else a) ta

null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: Foldable t => t a -> Int
length' = foldr (\_ b -> b + 1) 0

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> b <> f a) mempty
main :: IO ()
main = do
  putStrLn "hello world"
