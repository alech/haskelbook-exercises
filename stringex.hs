module StringExercises where

d1 :: [a] -> [a]
d1 = drop 1

addExcl :: String -> String
addExcl s = s ++ "!"

five :: String -> Char
five s = s !! 4

d9 :: [a] -> [a]
d9 = drop 9

thirdLetter :: String -> Char
thirdLetter s = s !! 3

letterIndex :: Int -> Char
letterIndex x = 
    s !! x
    where
        s = "Curry is awesome!"

rvrs :: String -> String
rvrs s =
    d9 s ++ (take 4 $ drop 5 s) ++ (take 5 s)
