module Phone where

import Data.Char (isLower, toLower)
import Data.List (intercalate, sortBy, nub)

type Digit   = Char
type Presses = Int
data KeyMapping = KeyMapping Digit (Digit, Presses)
    deriving (Show, Eq)

data PhoneLayout = PhoneLayout {
      capsKey :: Digit
    , keyMap  :: [KeyMapping]
    } deriving (Show, Eq)

defaultLayout :: PhoneLayout
defaultLayout = PhoneLayout {
      capsKey = '*' :: Digit
    , keyMap  = [
          KeyMapping '1' ('1', 1) -- or 4?
        , KeyMapping 'a' ('2', 1)
        , KeyMapping 'b' ('2', 2)
        , KeyMapping 'c' ('2', 3)
        , KeyMapping '1' ('2', 4)
        , KeyMapping 'd' ('3', 1)
        , KeyMapping 'e' ('3', 2)
        , KeyMapping 'f' ('3', 3)
        , KeyMapping '2' ('3', 4)
        , KeyMapping 'g' ('4', 1)
        , KeyMapping 'h' ('4', 2)
        , KeyMapping 'i' ('4', 3)
        , KeyMapping '4' ('4', 4)
        , KeyMapping 'j' ('5', 1)
        , KeyMapping 'k' ('5', 2)
        , KeyMapping 'l' ('5', 3)
        , KeyMapping '5' ('5', 4)
        , KeyMapping 'm' ('6', 1)
        , KeyMapping 'n' ('6', 2)
        , KeyMapping 'o' ('6', 3)
        , KeyMapping '6' ('6', 4)
        , KeyMapping 'p' ('7', 1)
        , KeyMapping 'q' ('7', 2)
        , KeyMapping 'r' ('7', 3)
        , KeyMapping 's' ('7', 4)
        , KeyMapping '7' ('7', 5)
        , KeyMapping 't' ('8', 1)
        , KeyMapping 'u' ('8', 2)
        , KeyMapping 'v' ('8', 3)
        , KeyMapping '8' ('8', 4)
        , KeyMapping 'w' ('9', 1)
        , KeyMapping 'x' ('9', 2)
        , KeyMapping 'y' ('9', 3)
        , KeyMapping 'z' ('9', 4)
        , KeyMapping '9' ('9', 5)
        , KeyMapping '^' ('*', 2) -- ?
        , KeyMapping '*' ('*', 3)
        , KeyMapping 'n' ('2', 3)
        , KeyMapping 'n' ('2', 3)
    ]
    }

convo :: [String]
convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Just making sure rofl ur turn"]

reverseTaps :: PhoneLayout -> Char -> [(Digit, Presses)]
reverseTaps layout c
    | isLower c = dpFromChar layout c
    | otherwise = (capsKey layout, 1) : dpFromChar layout (toLower c)
    -- maybe use Maybe :)
    where
        dpFromChar :: PhoneLayout -> Char -> [(Digit, Presses)]
        dpFromChar layout c =
            map (\(KeyMapping _ r) -> r) $ filter (\(KeyMapping ch _) -> ch == c) $ keyMap layout

dpsFromString :: PhoneLayout -> String -> [(Digit, Presses)]
dpsFromString layout = concatMap (reverseTaps layout)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . (map snd)

mostPopularLetter :: String -> Char
--mostPopularLetter = fst . head . sortBy (\a b -> snd b `compare` snd a) . count
mostPopularLetter = mostPopular

-- same in general
mostPopular :: Eq a => [a] -> a
mostPopular = fst . head . sortBy (\a b -> snd b `compare` snd a) . count

-- previous "manual" attempt
--mostPopularLetter str =
--    fst . head $ sortBy (\a b -> snd b `compare` snd a) $ nub $
--        map (\c -> (c, length $ filter (==c) str)) str

-- given a list of as, return a list of (a, i) where i is the number
-- of occurences of a in the list. Needs a to have an Eq type class
-- instance
count :: Eq a => [a] -> [(a, Int)]
count = foldr (\a b -> if a `elem` map fst b then
                    map (\(e, i) -> if e == a then (e, i+1) else (e, i)) b
                else
                    (a, 1) : b
              )[]

coolestLetter :: [String] -> Char
coolestLetter = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopular . concat . map words

main :: IO ()
main = do
    putStrLn $ intercalate "\n" $
        map (\c -> c ++ "\n" ++
                   show (dpsFromString defaultLayout c) ++ "\n" ++
                   show (mostPopularLetter c) ++ "\n" ++
                   show (fingerTaps $ dpsFromString defaultLayout [mostPopularLetter c]) ++ "\n"
        ) convo
    putStrLn $ show (coolestLetter convo)
    putStrLn $ show (coolestWord convo)
