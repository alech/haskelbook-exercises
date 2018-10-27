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

main :: IO ()
main = do
    quickBatch $ monad (Second ((), "bar", "baz") :: Sum String ((), String, String))
    quickBatch $ monad (NopeDotJpg :: Nope ((), String, String))
