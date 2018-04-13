module Upper where

import Data.Char (isUpper, toUpper)

filterUpper :: String -> String
filterUpper = filter isUpper

capitalizeFirst :: String -> String
capitalizeFirst "" = ""
capitalizeFirst (x:xs) = toUpper x : xs

capitalizeAll :: String -> String
capitalizeAll "" = ""
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

firstCapitalized :: String -> Maybe Char
firstCapitalized "" = Nothing
firstCapitalized xs = Just $ toUpper $ head xs

firstCapitalized' :: String -> Maybe Char
firstCapitalized' = fmap toUpper . safeHead

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just $ x
