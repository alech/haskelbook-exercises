module ReplaceThe where

import Data.Maybe

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe s = unwords $ r' $ map notThe $ words s
    where
        r' :: [Maybe String] -> [String]
        r' [] = []
        r' ((Just s):xs) = s   : r' xs
        r' (Nothing :xs) = "a" : r' xs
