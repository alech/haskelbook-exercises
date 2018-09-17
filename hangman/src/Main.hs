module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- type WordList = [String]
newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where
        gameLength w =
            let
                l = length (w :: String)
            in
                   l >= minWordLength
                && l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, (length wl) - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
    Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
    show (Puzzle _ discovered guessed incorrect) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
        " Guessed so far: " ++ guessed ++
        "\nIncorrect guesses: " ++ show incorrect

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (const Nothing <$> s) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) ch = ch `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) ch = ch `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

fillInCharacter :: Puzzle -> Char -> Bool -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s i) c wasInWord =
    Puzzle word newFilledInSoFar (c : s) newI
    where
        zipper guessed wordChar guessChar =
            if wordChar == guessed then
                Just wordChar
            else
                guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar
        newI = if wasInWord then i else i + 1

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that char, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word, filling it in."
            return $ fillInCharacter puzzle guess True
        (False, _) -> do
            putStrLn "This character was not in the word, try again."
            return $ fillInCharacter puzzle guess False

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed inc) =
    if (inc) > (length wordToGuess) then
        do
            putStrLn "You lose!"
            putStrLn $ "The word was: " ++ wordToGuess
            exitSuccess
    else
        return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _ _) =
    if all isJust filledInSoFar then
        do
            putStrLn $ "You win, " ++ wordToGuess ++ " is correct."
            exitSuccess
    else
        return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin  puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
