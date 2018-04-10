module FearfulSymmetry where

myWords :: String -> [String]
myWords s
    | null s    = []
    | otherwise = word : myWords (drop ((length word) + length leadingSpaces + 1) s)
    where
        leadingSpaces = takeWhile (==' ') s
        word = takeWhile (/=' ') $ dropWhile (==' ') s

myLines :: String -> [String]
myLines s
    | null s    = []
    | otherwise = word : myLines (drop ((length word) + length leadingNewlines + 1) s)
    where
        leadingNewlines = takeWhile (=='\n') s
        word = takeWhile (/='\n') $ dropWhile (=='\n') s

mySplitAt :: Char -> String -> [String]
mySplitAt c s
    | null s    = []
    | otherwise = word : mySplitAt c (drop ((length word) + length leadingNewlines + 1) s)
    where
        leadingNewlines = takeWhile (==c) s
        word = takeWhile (/=c) $ dropWhile (==c) s

myLines' = mySplitAt '\n'
myWords' = mySplitAt ' '

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual =
	[ "Tyger Tyger, burning bright"
	, "In the forests of the night"
	, "What immortal hand or eye"
	, "Could frame thy fearful symmetry?"
	]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
