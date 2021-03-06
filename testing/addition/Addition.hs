module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

multRecSum :: (Integral a, Ord a) => a -> a -> a
multRecSum n 0 = 0
multRecSum n m =
    if m > 0 then
        n + multRecSum n (m - 1)
    else
        negate $ multRecSum n (-m)

main :: IO ()
main = hspec $ do
    describe "Division" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
    describe "Multiplication using recursive summing" $ do
        it "5 * 4 = 20" $ do
            multRecSum 5 4 `shouldBe` 20
        it "-5 * 7 = -35" $ do
            multRecSum (-5) 7 `shouldBe` (-35)
        it "5 * -11 = -55" $ do
            multRecSum 5 (-11) `shouldBe` (-55)
        it "-2 * -2 = 4" $ do
            multRecSum (-2) (-2) `shouldBe` 4
        it "0 * 100 = 0" $ do
            multRecSum 0 100 `shouldBe` 0
        it "100 * 0 = 0" $ do
            multRecSum 100 0 `shouldBe` 0
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
