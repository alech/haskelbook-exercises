module ChapterExercises where

import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m)
                  => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc =
    Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

type IdentityAssoc =
    Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')
    
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    -- Applicatives FTW!
    -- :t (<*>)
    -- Applicative f => f (a -> b) -> f a -> f b
    -- :t Two <$> arbitrary
    -- Gen (b -> Two a b)
    -- :t arbitrary (the second one): Gen b
    -- => :t Two <$> arbitrary <*> arbitrary == Gen (Two a b)
    arbitrary = Two <$> arbitrary <*> arbitrary

type TwoAssoc =
    Two [Int] String -> Two [Int] String -> Two [Int] String -> Bool

data Three a b c = Three a b c
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)
    => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')
    
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    -- Applicatives FTW!
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

type ThreeAssoc =
    Three [Int] String  Ordering -> Three [Int] String Ordering -> Three [Int] String Ordering -> Bool

data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
    => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a' b' c' d') =
        Four (a <> a') (b <> b') (c <> c') (d <> d')
    
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
    -- Applicatives FTW!
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type FourAssoc =
    Four [Int] String Ordering [Bool] ->
    Four [Int] String Ordering [Bool] ->
    Four [Int] String Ordering [Bool] ->
    Bool

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: FourAssoc)
