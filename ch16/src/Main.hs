module Main where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
       f a
    -> Fun a b
    -> Fun b c
    -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a
    
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary
    
data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
     => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = do
    quickCheck (functorIdentity :: Identity String -> Bool)
    quickCheck (functorCompose :: Identity String -> Fun String Int -> Fun Int Int -> Bool)
    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck (functorCompose :: Pair String -> Fun String Int -> Fun Int Int -> Bool)
    quickCheck (functorIdentity :: Two Int String -> Bool)
    quickCheck (functorCompose :: Two Int String -> Fun String Int -> Fun Int Int -> Bool)
    quickCheck (functorIdentity :: Three Int String String -> Bool)
    quickCheck (functorCompose :: Three Int Int String -> Fun String Int -> Fun Int Int -> Bool)
    quickCheck (functorIdentity :: Three' Int String -> Bool)
    quickCheck (functorCompose :: Three' Int String -> Fun String Int -> Fun Int Int -> Bool)
    quickCheck (functorIdentity :: Four Int String Int String -> Bool)
    quickCheck (functorCompose :: Four Int String Int String -> Fun String Int -> Fun Int Int -> Bool)
    quickCheck (functorIdentity :: Four' Int String -> Bool)
    quickCheck (functorCompose :: Four' Int String -> Fun String Int -> Fun Int Int -> Bool)
