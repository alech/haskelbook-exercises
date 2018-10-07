module Cipher where

import Data.Char
import Test.QuickCheck
import Debug.Trace

caesar :: Int -> Char -> Char
caesar i c
    | c `elem` enumFromTo 'a' 'z' = chr (ord 'a' + ((ord c - ord 'a' + i) `mod` 26))
    | c `elem` enumFromTo 'A' 'Z' = chr (ord 'A' + ((ord c - ord 'A' + i) `mod` 26))
    | otherwise                   = c -- do not "encrypt" nonalpha

unCaesar :: Int -> Char -> Char
unCaesar i = caesar (26 - i)

rot13 :: String -> String
rot13 = map (caesar 13)

charToShift :: Char -> Int
charToShift c
    | c `elem` enumFromTo 'a' 'z' = ord c - ord 'a'
    | c `elem` enumFromTo 'A' 'Z' = ord c - ord 'A'
    | otherwise                   = 0

vigenere :: String -> String -> String
vigenere "" plain      = plain
vigenere keyword plain =
    zipWith (\a b -> caesar (charToShift a) b) (mconcat $ repeat keyword) plain

unVigenere :: String -> String -> String
unVigenere "" cipher      = cipher
unVigenere keyword ciphertext =
    zipWith (\a b -> unCaesar (charToShift a) b) (mconcat $ repeat keyword) ciphertext

-- TODO rewrite with liftM2 if possible?
genCharAndInt :: Gen (Char, Int)
genCharAndInt = do
    c <- arbitrary :: Gen Char
    i <- arbitrary :: Gen Int
    return (c, i)

genStringString :: Gen (String, String)
genStringString = do
    s1 <- arbitrary :: Gen String
    s2 <- arbitrary :: Gen String
    return (s1, s2)

prop_caesarUnCaesarIsIdentity :: Property
prop_caesarUnCaesarIsIdentity =
    forAll genCharAndInt (\(c, i) -> (unCaesar i $ caesar i c) == c)

prop_vigenereUnvigenereIsIdentity :: Property
prop_vigenereUnvigenereIsIdentity =
    -- trace example for debugging
    --forAll genStringString  (\(s1, s2) -> trace ("s1: " ++ s1 ++ ", s2: " ++ s2) $ unVigenere s1 (vigenere s1 s2) == s2)
    forAll genStringString  (\(s1, s2) -> unVigenere s1 (vigenere s1 s2) == s2)

main :: IO ()
main = do
    line <- getLine
    putStrLn $ "rot13: " ++ rot13 line
    putStrLn $ "vigenere vigenere: "++ vigenere "vigenere" line
