module OptionalFirst where

import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) =>
    m -> m -> m -> Bool
monoidAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

data Optional a =
      Nada
    | Only a
    deriving (Eq, Show)

newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Semigroup (First' a) where
    (<>) f1 f2 = case getFirst' f1 of
        Only x  -> First' { getFirst' = Only x }
        Nada    -> First' { getFirst' = getFirst' f2 }

instance Monoid (First' a) where
    mempty = First' { getFirst' = Nada }

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = frequency [
         (1, return Nada)
         -- arbitrary returns something of type Gen a, Only is
         -- a function from a to Optional a, so
         -- Only <$> arbitrary is of type Gen (Optional a)
       , (10, Only <$> arbitrary)
        ]

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        x <- arbitrary
        return $ First' { getFirst' = x }

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)

