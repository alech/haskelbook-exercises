module Main where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (isUpper, isLower, toLower)

isPalindrome :: String -> Bool
isPalindrome s =
    filtered == reverse filtered
    where
        filtered = toLower <$> filter (\c -> isUpper c || isLower c) s

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (isPalindrome line1) of
        True  -> putStrLn "It's a palindrome!"
        False -> do
            putStrLn "Nope!"
            exitSuccess

main :: IO ()
main = palindrome
