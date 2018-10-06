module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (vector)
import Test.QuickCheck.Monadic (monadicIO, run, pick, assert)
import Data.Set (Set(..), fromList, (\\), toList)

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
    Puzzle String [Maybe Char] [Char] Int deriving Eq

instance Show Puzzle where
    show (Puzzle _ discovered guessed incorrect) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
        " Guessed so far: " ++ guessed ++
        "\nIncorrect guesses: " ++ show incorrect

genEmptyPuzzle :: Gen Puzzle
genEmptyPuzzle = do
    -- get a random word length between min and max
    wordLength <- elements [minWordLength..maxWordLength]
    -- use traverse to turn [Gen Char] into Gen [Char]
    word <- traverse (\_ -> elements ['a'..'z']) (replicate wordLength ())
    return $ freshPuzzle word

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

genPuzzleAndCorrectChar :: Gen (Puzzle, Char)
genPuzzleAndCorrectChar = do
    puzzle <- genEmptyPuzzle
    let correctChars = (\(Puzzle s _ _ _) -> s) puzzle
    char <- elements correctChars
    return (puzzle, char)

genPuzzleAndIncorrectChar :: Gen (Puzzle, Char)
genPuzzleAndIncorrectChar = do
    puzzle <- genEmptyPuzzle
    let correctChars   = fromList $ (\(Puzzle s _ _ _) -> s) puzzle
    let incorrectChars = toList (fromList ['a'..'z'] \\ correctChars)
    char <- elements incorrectChars
    return (puzzle, char)

prop_fillInCorrectCharacter :: Property
prop_fillInCorrectCharacter =
    forAll genPuzzleAndCorrectChar
        (\(p@(Puzzle word discovered guessed incorrect), c) ->
        -- incorrect value does not change on correct guess
        pIncorrect (newPuzzle p c) == incorrect &&
        -- and 'Just c' is in the discovered list
        (Just c) `elem` pDiscovered (newPuzzle p c))
    where
        newPuzzle p c = fillInCharacter p c True
        pIncorrect  (Puzzle _ _ _ i) = i
        pDiscovered (Puzzle _ d _ _) = d

prop_fillInIncorrectCharacter :: Property
prop_fillInIncorrectCharacter =
    forAll genPuzzleAndIncorrectChar
        (\(p@(Puzzle word discovered guessed incorrect), c) ->
        -- incorrect value increases on icorrect guess
        pIncorrect (newPuzzle p c) == incorrect + 1 &&
        -- and discovered list should stay the same
        pDiscovered (newPuzzle p c) == discovered)
    where
        newPuzzle p c = fillInCharacter p c False
        pIncorrect  (Puzzle _ _ _ i) = i
        pDiscovered (Puzzle _ d _ _) = d

prop_handleGuessedAlreadyGuessed :: Property
prop_handleGuessedAlreadyGuessed = monadicIO $ do
    (p, c) <- pick genPuzzleAndCorrectChar
    let guessedCPuzzle = fillInCharacter p c True
    handledPuzzle <- run $ handleGuess guessedCPuzzle c
    -- assert that puzzle did not change
    assert $ handledPuzzle == guessedCPuzzle

prop_handleGuessedGuessCorrect :: Property
prop_handleGuessedGuessCorrect = monadicIO $ do
    (p, c) <- pick genPuzzleAndCorrectChar
    handledPuzzle <- run $ handleGuess p c
    -- TODO this is actually just the same as in the code ... better test?
    assert $ handledPuzzle == fillInCharacter p c True

prop_handleGuessedGuessIncorrect :: Property
prop_handleGuessedGuessIncorrect = monadicIO $ do
    (p, c) <- pick genPuzzleAndIncorrectChar
    handledPuzzle <- run $ handleGuess p c
    -- asser that incorrect counter increased
    assert $ (incorrect p) + 1 == incorrect handledPuzzle
    where
        incorrect (Puzzle _ _ _ i) = i

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
