module Main where

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ fmap f ma 

data Sum a b =
      First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure    = Second
    (First a)  <*> _        = First a
    (Second _) <*> First a  = First a
    (Second f) <*> Second x = Second $ f x

instance Monad (Sum a) where
    return = pure
    (First a)  >>= _ = First a
    (Second x) >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [First x, Second y]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    First x  =-= First y  = eq x y
    Second x =-= Second y = eq x y
    _        =-= _        = eq True False

data Nope a = NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _  = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    return _ = NopeDotJpg
    _ >>= _  = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
    _ =-= _ = eq True True

data PhhhbbtttEither b a =
      LeftP a
    | RightP b
    deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
    fmap f (LeftP a)  = LeftP $ f a
    fmap _ (RightP b) = RightP b

instance Applicative (PhhhbbtttEither b) where
    pure    = LeftP
    (LeftP f) <*> (LeftP x) = LeftP $ f x
    (LeftP _) <*> RightP x  = RightP x
    (RightP x) <*> _        = RightP x

instance Monad (PhhhbbtttEither b) where
    return = pure
    (LeftP a)  >>= f = f a
    (RightP b) >>= _ = RightP b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [LeftP x, RightP y]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
    LeftP x  =-= LeftP y  = eq x y
    RightP x =-= RightP y = eq x y
    _        =-= _        = eq True False

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    x =-= y = eq x y

data List a =
      Nil
    | Cons a (List a)
    deriving (Show, Eq)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil         <*> _           = Nil
    (Cons f fs) <*> xs = appendLists (fmap f xs) (fs <*> xs)

appendLists :: List a -> List a -> List a
appendLists Nil ys         = ys
appendLists (Cons x xs) ys = Cons x (appendLists xs ys)

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

instance Monad List where
    return = pure
    Nil         >>= _ = Nil
    (Cons x xs) >>= f = appendLists (f x) (xs >>= f)

j :: Monad m => m (m a) -> m a
j mma = mma >>= id -- or "join"

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = fmap f ma <*> mb

-- ap
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh [] _     = return []
meh (x:xs) f = (:) <$> f x <*> meh xs f

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id

main :: IO ()
main = do
    quickBatch $ monad (Second ((), "bar", "baz") :: Sum String ((), String, String))
    quickBatch $ monad (NopeDotJpg :: Nope ((), String, String))
    quickBatch $ monad (RightP ("abc", "foo", "bar") :: PhhhbbtttEither (String, String, String) (String, String, String))
    quickBatch $ monad (Identity ("abc", "foo", "bar") :: Identity (String, String, String))
    quickBatch $ monad (Cons ("abc", "foo", "bar") Nil :: List (String, String, String))
