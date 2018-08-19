module AsPatterns where

import Data.Char (toUpper)
import Data.List (intercalate)

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = True
isSubseqOf (x:xrest) ys@(_:yrest)
    | x `elem` ys = isSubseqOf xrest yrest
    | otherwise   = False

-- as pattern also work in lambdas
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\w@(x:xs) -> (w, toUpper x : xs)) . words

capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph =
    intercalate " " . cap . words
    where
        cap :: [String] -> [String]
        cap [] = []
        cap (x:xs) = capitalizeWord x : capWordIfPrevDot x xs
        capWordIfPrevDot :: String -> [String] -> [String]
        capWordIfPrevDot _        []     = []
        capWordIfPrevDot prevWord (x:xs)
            | '.' `elem` prevWord = capitalizeWord x : capWordIfPrevDot x xs
            | otherwise           = x : capWordIfPrevDot x xs

-- alternative using splitOn from Data.Text
-- intercalate (pack ". ") $ Prelude.map (pack .capitalizeWords .unpack) $ splitOn (pack ". ") $ pack s
