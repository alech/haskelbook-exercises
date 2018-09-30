{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module QuickCheckEx where

import Test.QuickCheck
import Data.List (sort)

half :: Fractional a => a -> a
half x = x / 2

halfIdentity = (*2) . half

prop_halfIdentity :: Property
prop_halfIdentity =
    forAll (arbitrary :: Gen Float) (\i -> halfIdentity i == i)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where
        go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)

prop_sortedListsAreOrdered :: Property
prop_sortedListsAreOrdered =
    forAll (arbitrary :: Gen [Int]) $ listOrdered . sort

plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z =
    x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y =
    x + y == y + x

multAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
multAssociative x y z =
    x * (y * z) == (x * y) * z

multCommutative :: (Num a, Eq a) => a -> a -> Bool
multCommutative x y =
    x * y == y * x

prop_associativity :: Property
prop_associativity =
    forAll (arbitrary :: Gen (Int, Int, Int))
        (\(x, y, z) -> plusAssociative x y z)

prop_commutativity :: Property
prop_commutativity =
    forAll (arbitrary :: Gen (Integer, Integer))
        $ uncurry plusCommutative

prop_multAssociativity :: Property
prop_multAssociativity =
    forAll (arbitrary :: Gen (Int, Int, Int))
        (\(x, y, z) -> multAssociative x y z)

prop_multCommutativity :: Property
prop_multCommutativity =
    forAll (arbitrary :: Gen (Integer, Integer))
        $ uncurry multCommutative 

quotRemLaw :: Integral a => a -> a -> Bool
quotRemLaw x y =
    quot x y * y + rem x y == x

divModLaw :: Integral a => a -> a -> Bool
divModLaw x y =
    div x y * y + mod x y == x

-- generate a pair of Integers where the second one is
-- guaranteed to be non-0 (for use with div, quot)
arbIntNonZeroInt :: Gen (Integer, Integer)
arbIntNonZeroInt = do
    a <- arbitrary :: Gen Integer
    b <- arbitrary :: Gen Integer
    if b == 0 then
        return (a, 1)
    else
        return (a, b)

prop_quotRemLaw :: Property
prop_quotRemLaw =
    forAll arbIntNonZeroInt $ uncurry quotRemLaw

prop_divModLaw :: Property
prop_divModLaw =
    forAll arbIntNonZeroInt $ uncurry divModLaw

powerAssociative :: Integral a => a -> a -> a -> Bool
powerAssociative x y z =
    x ^ (y ^ z) == (x ^ y) ^ z

powerCommutative :: Integral a => a -> a -> Bool
powerCommutative x y =
    x ^ y == y ^ x
    
arbThreePositiveIntegers :: Gen (Integer, Integer, Integer)
arbThreePositiveIntegers = do
    a <- arbitrary :: Gen Integer
    b <- arbitrary :: Gen Integer
    c <- arbitrary :: Gen Integer
    return (abs a, abs b, abs c)

arbTwoPositiveIntegers :: Gen (Integer, Integer)
arbTwoPositiveIntegers = do
    a <- arbitrary :: Gen Integer
    b <- arbitrary :: Gen Integer
    return (abs a, abs b)

prop_powerAssociative :: Property
prop_powerAssociative =
    forAll arbThreePositiveIntegers
    (\(x, y, z) -> powerAssociative x y z)

prop_powerCommutative :: Property
prop_powerCommutative =
    forAll arbTwoPositiveIntegers $ uncurry powerCommutative

reverseTwice :: Eq a => [a] -> Bool
reverseTwice x = (reverse . reverse) x == x

prop_reverseTwiceIsId :: Property
prop_reverseTwiceIsId =
    forAll (arbitrary :: Gen [Int]) reverseTwice

-- this is a bit of a hack in order to be able to
-- use a "arbitrary :: Int -> Int", which would need
-- a show instance for "forAll"
-- TODO: figure out if there are better ways to do this
instance Show (Int -> Int) where
    show _ = "f :: Int -> Int"

genFunctionIntIntAndInt :: Gen (Int -> Int, Int)
genFunctionIntIntAndInt = do
    f <- arbitrary :: Gen (Int -> Int)
    a <- arbitrary :: Gen Int
    return (f, a)

prop_dollar :: Property
prop_dollar =
    forAll (genFunctionIntIntAndInt) (\(f, a) -> (f $ a) == f a)

prop_foldr1 :: Property
prop_foldr1 =
    forAll (arbitrary :: Gen ([String], [String]))
        (\(a, b) -> foldr (:) a b == (++) a b)

prop_foldr2 :: Property
prop_foldr2 =
    forAll (arbitrary :: Gen [[String]])
        (\a -> foldr (++) [] a == concat a)

prop_length :: Property
prop_length =
    forAll (arbitrary :: Gen (Int, [String]))
        (\(n, xs) -> length (take n xs) == n)

prop_readShow :: Property
prop_readShow =
    forAll (arbitrary :: Gen Integer)
        (\x -> read (show x) == x)

main :: IO ()
main = do
    quickCheck prop_halfIdentity
    quickCheck prop_sortedListsAreOrdered
    quickCheck prop_associativity
    quickCheck prop_commutativity
    quickCheck prop_multAssociativity
    quickCheck prop_multCommutativity
    quickCheck prop_quotRemLaw
    quickCheck prop_divModLaw
    quickCheck prop_powerAssociative
    quickCheck prop_powerCommutative
    quickCheck prop_reverseTwiceIsId
    quickCheck prop_dollar
    quickCheck prop_foldr1
    quickCheck prop_foldr2
    quickCheck prop_length
    quickCheck prop_readShow
