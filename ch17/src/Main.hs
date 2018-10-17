module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Debug.Trace

data List a =
      Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

appendLists :: List a -> List a -> List a
appendLists Nil ys         = ys
appendLists (Cons x xs) ys = Cons x (appendLists xs ys)

repeat' :: a -> List a
repeat' x = xs
    where
        xs = Cons x xs

instance Applicative List where
    pure x = Cons x Nil
    Nil         <*> _           = Nil
    (Cons f fs) <*> xs = appendLists (fmap f xs) (fs <*> xs)

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

take' :: Int -> List a -> List a
take' n l =
    go n l Nil
    where
        go 0 _ res      = res
        go n Nil res    = res  
        go n (Cons x xs) res = if n < 0 then
                                    Nil
                               else
                                    go (n-1) xs (Cons x res)

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = let
                    (ZipList' l) = xs
                  in
                    take' 3000 l
            ys' = let
                    (ZipList' l) = ys
                  in
                    take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) =
        ZipList' $ fmap f xs 

zlAppend :: ZipList' a -> ZipList' a -> ZipList' a
zlAppend (ZipList' a) (ZipList' b) = ZipList' $ a `appendLists` b

instance Applicative ZipList' where
    pure x  = ZipList' (Cons x Nil)
    ZipList' Nil         <*> _                    = ZipList' Nil
    _                    <*> ZipList' Nil         = ZipList' Nil
    ZipList' (Cons f fs) <*> ZipList' (Cons x xs) = ZipList' (Cons (f x) Nil) `zlAppend` ((ZipList' fs) <*> (ZipList' xs))

main :: IO ()
main = do
    quickBatch (applicative (Cons ("b", "w", 1 :: Integer) Nil))
    --quickBatch (applicative (ZipList' $ Cons ("b", "w", 1 :: Integer) Nil))
