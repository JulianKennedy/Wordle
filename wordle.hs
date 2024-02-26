import System.Random (randomRIO)
import Control.Monad (when, unless)
import Data.Char (toLower)
import Data.List (intersect, nub)

-- Attribution: This project was created with the help of GitHub copilot and ChatGPT

-- Reads a list of words from a file
readWords :: FilePath -> IO [String]
readWords path = do
    contents <- readFile path
    return $ lines contents

-- Selects a random word from a list
getTargetWord :: [String] -> IO String
getTargetWord possibleWords = do
    randomIndex <- randomRIO (0, length possibleWords - 1)
    return (possibleWords !! randomIndex)

-- Counts the frequency of a character in a String
count :: String -> Char -> Int
count string char = length (filter (==char) string)

-- Replace an index of a String
-- Parameters: word, index, newChar
replace :: String -> Int -> Char -> String
replace [] _ _ = []
replace (h:t) 0 c = c:t
replace (h:t) index newChar =
  h: replace t (index-1) newChar 

-- Attribution: from Assignment1 class solutions
-- Remove duplicates
-- Parameters: list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (h:t) 
    | elem h t = removeDuplicates t 
    | otherwise = h : (removeDuplicates t)

-- Get index in a string 
getByIndex :: String -> Int -> Char
getByIndex [] _ = '~'
getByIndex (h:t) 0 = h
getByIndex (h:t) index = getByIndex t (index - 1)

-- Attribution: from TwentyQs Assignment 3 class solutions
getLineFixed =
   do
     line <- getLine
     return (fmap toLower (fixdel2 line))

-- Grab the guess from the user input and check that it is in the list of valid words
getGuess :: [String] -> IO String
getGuess allowedWords = do
  putStrLn "Enter your guess:"
  guess <- getLineFixed
  if guess `notElem` allowedWords then do
    putStrLn "Word not allowed, try again."
    getGuess allowedWords
  else
    return guess

-- Attribution: from TwentyQs Assignment 3 class solutions
-- fixdel2 deleted elements from string 
fixdel2 :: [Char] -> [Char]
fixdel2 st = fst (remdel2 st)
-- remdel2 st   returns (resulting_string, number_of_deletes_to_do)
remdel2 :: [Char] -> ([Char], Int)
remdel2 [] = ([],0)
remdel2 ('\DEL':t) = (s,n+1)
    where (s,n) = remdel2 t
remdel2 (h:t)
    | n>0 = (s,n-1)
    | otherwise = (h:s,0)
    where (s,n) = remdel2 t


-- Map take guess to wordle box value
-- Parameters: guessChar targetchar targetWord
mapChar :: Char -> Char -> String -> Char
mapChar guessChar targetChar targetWord = 
    if guessChar == targetChar then '🟩'
    else if guessChar `elem` targetWord then '🟨'
    else '⬜'

-- Apply colors greedy
-- Parameters: guess word, target word
applyColorsGreedy :: String -> String -> String
applyColorsGreedy guessWord targetWord = applyColorsGreedyHelper guessWord targetWord targetWord

applyColorsGreedyHelper :: String -> String -> String -> String
applyColorsGreedyHelper [] _ _ = []
applyColorsGreedyHelper _ [] _ = []
applyColorsGreedyHelper _ _ [] = []
applyColorsGreedyHelper (h1:t1) (h2:t2) targetWord = (mapChar h1 h2 targetWord):(applyColorsGreedyHelper t1 t2 targetWord)


-- TEST: putStrLn (removeYellowIfNecessary "daddy" "dader" "🟩🟩🟩🟨⬜") = 🟩🟩🟩⬜⬜
-- remove yellow boxes if necessary for duplicated letters
-- Parameters: guess, target, greedy result
removeYellowIfNecessary :: String -> String -> String -> String
removeYellowIfNecessary guess target greedyResult = foldl (\acc char -> removeYellowByChar guess target acc char) greedyResult (removeDuplicates guess)

-- if there are yellows to remove for specific char, this will do that
-- Parameters: guess target result char
removeYellowByChar :: String -> String -> String -> Char -> String
removeYellowByChar guess target result char = 
    let guessFreq = (count guess char)
        targetFreq = (count target char)
        numYellowToRemove = (max (guessFreq - targetFreq) 0)
    in removeNYellows guess result char numYellowToRemove 4
 

-- remove numYellowToRemove yellows from the end of a string for a given character
-- Parameters: guess result char numYellowToRemove index
removeNYellows :: String -> String -> Char -> Int -> Int -> String
removeNYellows _ result _ _ (-1) = result
removeNYellows _ result _ 0 _ = result
removeNYellows guess result char numYellowToRemove index =
    if (((getByIndex guess index) == char) && ((getByIndex result index) == '🟨')) then (removeNYellows guess (replace result index '⬜') char (numYellowToRemove - 1) (index - 1))
    else (removeNYellows guess result char numYellowToRemove (index - 1))


-- Parameters: guess target
displayFeedback :: String -> String -> IO ()
displayFeedback guess target = do
    let initialResult = applyColorsGreedy guess target
    let filteredResult = removeYellowIfNecessary guess target initialResult
    putStrLn filteredResult





-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
------------------------------ SOLVE MODE -----------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------  

filterForContains :: [String] -> Char -> Int -> [String]
filterForContains low char index = [word | word <- low, char `elem` word && (word !! index) /= char]

filterForNotContains :: [String] -> Char -> [String]
filterForNotContains low char = [word | word <- low, char `notElem` word]

filterForPosition :: [String] -> Char -> Int -> [String]
filterForPosition low char index = [word | word <- low, (word !! index) == char]

filterFor :: [String] -> String -> String -> Int -> [String]
filterFor low word result index = 
    if (result !! index) == '⬜' then filterForNotContains low (word !! index)
    else if result !! index == '🟨' then filterForContains low (word !! index) index
    else filterForPosition low (word !! index) index

filterr :: [String] -> String -> String -> [String]
filterr low word result = foldl (\acc i -> filterFor acc word result i) low [0..4]

-- Parameters: possibleWords, words, results
-- Returns: all words that fit words and their respective results
getValidWords :: [String] -> [String] -> [String] -> [String]
getValidWords low words results = foldl (\acc i -> filterr acc (words !! i) (results !! i)) low [0..((length words) - 1)]

-- calculateEntropyHelper iterates over all the partitions of a word and calculates the entropy of the word
-- Parameters word guesses results possibleWords partitions validWordsSize runningTotal
calculateEntropyHelper :: String -> [String] -> [String] -> [String] -> [String] -> Double -> Double -> Double
calculateEntropyHelper _ _ _ _ [] _ runningTotal = runningTotal
calculateEntropyHelper word guesses results possibleWords (h:t) validWordsSize runningTotal =
    let validWordCount = fromIntegral (length (getValidWords possibleWords (guesses ++ [word]) (results ++ [h])))
        p = validWordCount / validWordsSize
        entropy = if p > 0.0 then p * (logBase 2 (1.0/p)) else 0.0
    in calculateEntropyHelper word guesses results possibleWords t validWordsSize (runningTotal + entropy)

-- calculateEntropy finds the entropy for each possible word
-- Parameters word guesses results
calculateEntropy :: String -> [String] -> [String] -> [String] -> Double
calculateEntropy word guesses results possibleWords =
    let partitionList = generateResultComb 0 [""]
        denominator = fromIntegral (length (getValidWords possibleWords guesses results))
    in calculateEntropyHelper word guesses results possibleWords partitionList denominator 0

-- generateResultComb creates the 243 different possibilities of colored square combinations
-- Parameters size currentResults
generateResultComb :: Int -> [String] -> [String]
generateResultComb 5 currentResults = currentResults
generateResultComb size currentResults = 
    let addWhite = [res ++ "⬜" | res <- currentResults]
        addYellow = [res ++ "🟨" | res <- currentResults]
        addGreen = [res ++ "🟩" | res <- currentResults] 
    in generateResultComb (size+1) (addWhite ++ addGreen ++ addYellow)

--printAndReturn :: [String] -> IO [String]
--printAndReturn xs = do
--    mapM_ putStrLn xs  -- Print each string using putStrLn
--    return xs

-- getWordWithHighestEntropy finds the word, value pair with the highest entropy from a list of words
-- Parameters: values, words
getWordWithHighestEntropy :: [String] -> [Double] -> (String, Double)
getWordWithHighestEntropy words values = foldl (\acc i -> if (values !! i) > (snd acc) then ((words !! i), (values !! i)) else acc) ("aaaaa", 0) [0..((length values) - 1)]

generateBestWord :: [String] -> [String] -> [String] -> (String, Double)
generateBestWord low guesses results =
    if (length (getValidWords low guesses results)) <= 2 then 
        let chosenWord = (getValidWords low guesses results) !! 0
            chosenWordEntropy = calculateEntropy chosenWord guesses results low 
        in (chosenWord, chosenWordEntropy)
    else 
        let entropyValues = [calculateEntropy word guesses results low | word <- low]
        in (getWordWithHighestEntropy low entropyValues)

-- Play
-- Parameters targetWord numGuesses allowedWords
play :: String -> Int -> [String] -> IO ()
play targetWord numGuesses allowedWords =
    if numGuesses == 7 then do 
        putStrLn ("You are out of guesses. The correct word is " ++ targetWord)
    else do
        let guess = getGuess allowedWords
        guessAsString <- guess
        let isTargetWord = guessAsString == targetWord
        if isTargetWord then do
            putStrLn "🟩🟩🟩🟩🟩"
            putStrLn ("You guessed the word in " ++ (show numGuesses) ++ " guesses")
            putStrLn "CONGRATS MATE"
        else do
            displayFeedback guessAsString targetWord
            play targetWord (numGuesses + 1) allowedWords

main :: IO ()
main = do
    possibleWords <- readWords "data/possible_words.txt"
    allowedWords <- readWords "data/allowed_words.txt"
    targetWord <- getTargetWord possibleWords
    putStrLn "Target word selected. Start guessing" 
    play targetWord 1 allowedWords