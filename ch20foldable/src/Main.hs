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

data Constant a b = Constant b
instance Foldable (Constant a) where
    foldr f b1 (Constant b2) = f b2 b1

data Two a b = Two a b
instance Foldable (Two a) where
    foldr f b1 (Two _ b2) = f b2 b1

data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldr f b1 (Three _ _ b2) = f b2 b1

data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldr f b1 (Three' _ b2 b3) = f b3 (f b2 b1)

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldr f b1 (Four' _ b2 b3 b4) = f b4 (f b3 (f b2 b1))

filterF :: (Applicative f, Foldable t, Monoid (f a))
           => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)

main :: IO ()
main = do
  putStrLn "hello world"
