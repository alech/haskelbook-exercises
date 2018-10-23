module Main where

import Test.QuickCheck (Arbitrary, arbitrary, elements, Gen, (.&&.))
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Debug.Trace
import Control.Applicative (liftA2)
import Data.Monoid (Sum)

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
    -- this is needed so identity works, we need as many
    -- identity functions as values
    pure x  = ZipList' $ repeat' x
    ZipList' Nil         <*> _                    = ZipList' Nil
    _                    <*> ZipList' Nil         = ZipList' Nil
    ZipList' (Cons f fs) <*> ZipList' (Cons x xs) = ZipList' (Cons (f x) Nil) `zlAppend` ((ZipList' fs) <*> (ZipList' xs))

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

instance Semigroup a => Semigroup (ZipList' a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList' a) where
    mempty = pure mempty

data Validation e a =
      Failure e
    | Success a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
    pure = Success
    (Failure e1) <*> (Failure e2) = Failure $ e1 <> e2
    (Failure e)  <*> (Success _)  = Failure e
    (Success _)  <*> (Failure e)  = Failure e
    (Success f)  <*> (Success x)  = Success $ f x

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Failure x, Success y]

instance (Eq a, Eq e) => EqProp (Validation e a) where
    -- is there a better way to write this than eq True False???
    Failure e1  =-= Failure e2  = eq e1 e2
    Success a1  =-= Success a2  = eq a1 a2
    _           =-= _           = eq True False

data Pair a = Pair a a
    deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair x1 x2) = Pair (f x1) (f x2)

instance Applicative Pair where
    pure x = Pair x x
    Pair f1 f2 <*> Pair x1 x2 = Pair (f1 x1) (f2 x2)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Pair a) where
    Pair a1 a2 =-= Pair b1 b2 = eq a1 b1 .&. eq a2 b2

data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    (Two a1 f) <*> (Two a2 x) = Two (a1 <> a2) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    Two a1 b1 =-= Two a2 b2 = eq a1 a2 .&. eq b1 b2

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (Three a1 b1 f) <*> (Three a2 b2 x) = Three (a1 <> a2) (b1 <> b2) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    Three a1 b1 c1 =-= Three a2 b2 c2 =
        eq a1 a2 .&. eq b1 b2 .&. eq c1 c2 

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Monoid a) => Applicative (Three' a) where
    pure x = Three' mempty x x
    (Three' a1 f1 f2) <*> (Three' a2 x1 x2) = Three' (a1 <> a2) (f1 x1) (f2 x2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    Three' a1 b1 c1 =-= Three' a2 b2 c2 =
        eq a1 a2 .&. eq b1 b2 .&. eq c1 c2 

data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (Four a1 b1 c1 f) <*> (Four a2 b2 c2 x) =
        Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    Four a1 b1 c1 d1 =-= Four a2 b2 c2 d2 =
        eq a1 a2 .&. eq b1 b2 .&. eq c1 c2  .&. eq d1 d2

main :: IO ()
main = do
    quickBatch (applicative (Cons ("b", "w", 1 :: Integer) Nil))
    quickBatch (applicative (ZipList' $ Cons ("b", "w", 1 :: Integer) Nil))
    quickBatch (applicative (Success ("b", "w", 1) :: Validation String (String, String, Sum Integer)))
    quickBatch (applicative (Pair ("b", "w", 1 :: Integer) ("b", "w", 1 :: Integer)))
    quickBatch (applicative (Two "foo" ("b", "w", 1 :: Integer)))
    quickBatch (applicative (Three "foo" [True] ("b", "w", 1 :: Integer)))
    quickBatch (applicative (Three' "foo" ("a", "", 1 :: Integer) ("b", "w", 1 :: Integer)))
    quickBatch (applicative (Four "foo" [True] [2 :: Int] ("b", "w", 1 :: Integer)))
