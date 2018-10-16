module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
      Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

concatLists :: List a -> List a -> List a
concatLists Nil ys         = ys
concatLists (Cons x xs) ys = Cons x (concatLists xs ys)

instance Applicative List where
    pure x = Cons x Nil
    Nil         <*> _           = Nil
    (Cons f fs) <*> xs = concatLists (fmap f xs) (fs <*> xs)

-- we are lazy for now and just do up to two-element lists
instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Nil, Cons x Nil, Cons x (Cons y Nil)]
    
instance Eq a => EqProp (List a) where
    -- is there a better way to write this than eq True True???
    Nil         =-= Nil         = eq True True
    (Cons x xs) =-= (Cons y ys) = eq x y .&&. (xs =-= ys)

main :: IO ()
main = 
    quickBatch (applicative (Cons ("b", "w", 1 :: Integer) Nil))
