module ValidateTheWord where

import Data.Maybe

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c = c `elem` vowels

countVowels :: String -> Int
countVowels = foldr (\a b -> if isVowel a then b + 1 else b) 0

newtype Word' =
    Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord w =
    if countVowels w > (length w - countVowels w) then
        Nothing
    else
        Just $ Word' w
