import System.Random (randomRIO)
import Control.Monad (when, unless)
import Data.Char (toLower)
import Data.List (intersect, nub)
import System.TimeIt


-- Attribution: This project was created with the help of GitHub copilot and ChatGPT


-- Purpose: Reads a list of words from a file
-- Parameters: file path
-- Returns: IO list of strings in the file
readWords :: FilePath -> IO [String]
readWords path = do
    contents <- readFile path
    return $ lines contents


-- Purpose: Selects a random word from a list
-- Parameters: list of all possible words
-- Returns: IO String of target word
getTargetWord :: [String] -> IO String
getTargetWord possibleWords = do
    randomIndex <- randomRIO (0, length possibleWords - 1)
    return (possibleWords !! randomIndex)


-- Purpose: Counts the frequency of a character in a String
-- Parameters: word to filter through, character to search for
-- Returns: number of times character appears in a word
count :: String -> Char -> Int
count string char = length (filter (==char) string)


-- Purpose: Replace an index of a String
-- Parameters: word, index, newChar
-- Returns: word with word[index] updated to newChar
replace :: String -> Int -> Char -> String
replace [] _ _ = []
replace (h:t) 0 c = c:t
replace (h:t) index newChar =
    h: replace t (index-1) newChar 


-- Attribution: from Assignment1 class solutions
-- Purpose: Removes duplicates from a list
-- Parameters: list
-- Returns: list with duplicates removed
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (h:t) 
    | elem h t = removeDuplicates t 
    | otherwise = h : (removeDuplicates t)


-- Attribution: from TwentyQs Assignment 3 class solutions
-- Purpose: removes deleted elements from input string and converts all capital letters to lowercase for error handling
-- Parameters: None
-- Returns: IO String with the corrected input and changes all to lower case
getLineFixed :: IO String
getLineFixed =
    do
        line <- getLine
        return (fmap toLower (fixdel2 line))


-- Purpose: Grab the guess from the user input and check that it is in the list of valid words
-- Parameters: List of allowed words
-- Returns: IO String of valid user word guess
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
-- Purpose: fixdel2 deleted elements from string 
-- Parameters: string (generally user input)
-- Returns: string where delete characters are dealt with correctly
fixdel2 :: [Char] -> [Char]
fixdel2 st = fst (remdel2 st)
remdel2 :: [Char] -> ([Char], Int)
remdel2 [] = ([],0)
remdel2 ('\DEL':t) = (s,n+1)
    where (s,n) = remdel2 t
remdel2 (h:t)
    | n>0 = (s,n-1)
    | otherwise = (h:s,0)
    where (s,n) = remdel2 t


-- Purpose: Map take guess to wordle box value
-- Parameters: char in gu
-- Returns: Associated result character (one of "🟩🟨⬜")
mapChar :: Char -> Char -> String -> Char
mapChar guessChar targetChar targetWord = 
    if guessChar == targetChar then '🟩'
    else if guessChar `elem` targetWord then '🟨'
    else '⬜'


-- Purpose: Apply colors greedily to all characers in the word
-- Parameters: word that was guessed by the user, target word that the user is trying to guess
-- Returns: String with result (combination of "🟩🟨⬜") applied greedily
applyColorsGreedy :: String -> String -> String
applyColorsGreedy guessWord targetWord = applyColorsGreedyHelper guessWord targetWord targetWord

-- This is just a helper function for applyColorsGreedy above
applyColorsGreedyHelper :: String -> String -> String -> String
applyColorsGreedyHelper [] _ _ = []
applyColorsGreedyHelper _ [] _ = []
applyColorsGreedyHelper _ _ [] = []
applyColorsGreedyHelper (h1:t1) (h2:t2) targetWord = (mapChar h1 h2 targetWord):(applyColorsGreedyHelper t1 t2 targetWord)


-- TEST: putStrLn (removeYellowIfNecessary "daddy" "dader" "🟩🟩🟩🟨⬜") = 🟩🟩🟩⬜⬜
-- Purpose: Remove yellow boxes if necessary for duplicated letters
-- Parameters: guess target greedy result
-- Returns: string with appropriate number of yellow squares
removeYellowIfNecessary :: String -> String -> String -> String
removeYellowIfNecessary guess target greedyResult = foldl (\acc char -> removeYellowByChar guess target acc char) greedyResult (removeDuplicates guess)


-- Purpose: if there are yellows to remove for specific char, this will do that. This is a complex edge case of wordle coloring.
-- Parameters: guess target result char
-- Returns: string with appropriate number of yellow squares for a certain character
removeYellowByChar :: String -> String -> String -> Char -> String
removeYellowByChar guess target result char = 
    let guessFreq = (count guess char)
        targetFreq = (count target char)
        numYellowToRemove = (max (guessFreq - targetFreq) 0)
    in removeNYellows guess result char numYellowToRemove 4
 

-- Purpose: remove numYellowToRemove yellows from the end of a string for a given character
-- Parameters: guess result char numYellowToRemove index
-- Returns: String with the last N 🟨 removed when they show up with a given character
removeNYellows :: String -> String -> Char -> Int -> Int -> String
removeNYellows _ result _ _ (-1) = result
removeNYellows _ result _ 0 _ = result
removeNYellows guess result char numYellowToRemove index =
    if (((guess !! index) == char) && ((result !! index) == '🟨')) then (removeNYellows guess (replace result index '⬜') char (numYellowToRemove - 1) (index - 1))
    else (removeNYellows guess result char numYellowToRemove (index - 1))


-- Purpose: Compares a guess and a target and prints the result to the console
-- Parameters: guess target
-- Returns: Nothing. Simply prints the result (in terms of "🟩🟨⬜") to the console
displayFeedback :: String -> String -> IO ()
displayFeedback guess target = do
    let initialResult = applyColorsGreedy guess target
    let filteredResult = removeYellowIfNecessary guess target initialResult
    putStrLn filteredResult


-- Purpose: This is the main function that runs the "play" game mode
-- Parameters: targetWord numGuesses allowedWords
-- Returns: Nothing. This is the main game loop for the "play" game mode
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



-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
------------------------------ SOLVE MODE -----------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------  


-- Purpose: filters a list of strings for those containing specific character
-- Parameters: list of words, character, index
-- Returns: A filtered list of words that contain the character but not at the parameter index
filterForContains :: [String] -> Char -> Int -> [String]
filterForContains low char index = [word | word <- low, char `elem` word && (word !! index) /= char]


-- Purpose: filters a list of strings for those that do not contain a specific character
-- Parameters: list of words, character
-- Returns: a filtered list of words that do not contain the parameter character
filterForNotContains :: [String] -> Char -> [String]
filterForNotContains low char = [word | word <- low, char `notElem` word]


-- Purpose: filters a list of strings for those that contain a specific character in a specific position
-- Parameters: list of words, character, index
-- Returns: a filtered list of words that contain the character at position index
filterForPosition :: [String] -> Char -> Int -> [String]
filterForPosition low char index = [word | word <- low, (word !! index) == char]


-- Purpose: applies a specific filter based on a 'result' character (one of "🟩🟨⬜")
-- Parameters: list of words, word, corresponding result, index in result
-- Returns: a list of strings filtered according to the type indicated by result at position index.
filterFor :: [String] -> String -> String -> Int -> [String]
filterFor low word result index = 
    if (result !! index) == '⬜' then filterForNotContains low (word !! index)
    else if result !! index == '🟨' then filterForContains low (word !! index) index
    else filterForPosition low (word !! index) index


-- Purpose: applies filters to a list of strings based on a word and its corresponding result
-- Parameters: list of words, word, corresponding result
-- Returns: A list of strings that satisfy the requirements of the result
filterr :: [String] -> String -> String -> [String]
filterr low word result = foldl (\acc i -> filterFor acc word result i) low [0..4]


-- Purpose: Filters for all valid words (from the list of possible words)
-- Parameters: list of possible words, words (guesses), results (corresponding to guesses)
-- Returns: all words that fit words and their respective results
getValidWords :: [String] -> [String] -> [String] -> [String]
getValidWords low words results = foldl (\acc i -> filterr acc (words !! i) (results !! i)) low [0..((length words) - 1)]


-- Purpose: calculateEntropy finds the entropy for each possible word
-- Parameters: word to find entropy of, list of guesses, list of corresponding results (to the guesses)
-- Returns: The entropy of a word given previous guesses and their corresponding results
calculateEntropy :: String -> [String] -> [String] -> [String] -> Double
calculateEntropy word guesses results possibleWords =
    let partitionList = generateResultComb 0 [""]
        denominator = fromIntegral (length (getValidWords possibleWords guesses results))
    in calculateEntropyHelper word guesses results possibleWords partitionList denominator 0

-- calculateEntropyHelper is a helper function for calculateEntropy. Should not be called directly.
calculateEntropyHelper :: String -> [String] -> [String] -> [String] -> [String] -> Double -> Double -> Double
calculateEntropyHelper _ _ _ _ [] _ runningTotal = runningTotal
calculateEntropyHelper word guesses results possibleWords (h:t) validWordsSize runningTotal =
    let validWordCount = fromIntegral (length (getValidWords possibleWords (guesses ++ [word]) (results ++ [h])))
        p = validWordCount / validWordsSize
        entropy = if p > 0.0 then p * (logBase 2 (1.0/p)) else 0.0
    in calculateEntropyHelper word guesses results possibleWords t validWordsSize (runningTotal + entropy)


-- Purpose: generateResultComb creates the 243 different possibilities of colored square combinations
-- Parameters: size currentResults
-- Returns: 
generateResultComb :: Int -> [String] -> [String]
generateResultComb 5 currentResults = currentResults
generateResultComb size currentResults = 
    let addWhite = [res ++ "⬜" | res <- currentResults]
        addYellow = [res ++ "🟨" | res <- currentResults]
        addGreen = [res ++ "🟩" | res <- currentResults] 
    in generateResultComb (size+1) (addWhite ++ addGreen ++ addYellow)


-- Purpose: getWordWithHighestEntropy finds the word, value pair with the highest entropy from a list of words
-- Parameters: values, words
-- Returns: The (word, entropy) with the highest entropy as a tuple
getWordWithHighestEntropy :: [String] -> [Double] -> (String, Double)
getWordWithHighestEntropy words values = foldl (\acc i -> if (values !! i) > (snd acc) then ((words !! i), (values !! i)) else acc) ("aaaaa", 0) [0..((length values) - 1)]


-- Purpose: Generates the next best word given previous guesses and results. This is done by using information theory and calculating entropy.
-- Parameters: list of possible words, guesses, corresponding results
-- Returns: The next best word and its entropy value in tuple form (nextBestWord, entropyValue)
generateBestWord :: [String] -> [String] -> [String] -> (String, Double)
generateBestWord low guesses results =
    if (length (getValidWords low guesses results)) <= 2 then 
        let chosenWord = (getValidWords low guesses results) !! 0
            chosenWordEntropy = calculateEntropy chosenWord guesses results low 
        in (chosenWord, chosenWordEntropy)
    else 
        let entropyValues = [calculateEntropy word guesses results low | word <- low]
        in (getWordWithHighestEntropy low entropyValues)


-- Purpose: Grab the result from the user input and check that it is a valid result. Incorrect results prompt user to resumbit.
-- Parameters: None
-- Returns: A valid result in the form of 5 characters (each one of "🟩🟨⬜")
getResult :: IO String
getResult = do
    putStrLn "Enter result: "
    result <- getLineFixed
    if result `notElem` (generateResultComb 0 [""]) then do
        putStrLn "Result not allowed, try again."
        getResult
    else
        return result

-- Purpose: Solve is the main loop function for "solve" mode
-- Parameters: possible words, guesses so far, results so far
-- Returns: Nothing. This is the game loop
solve :: [String] -> [String] -> [String] -> IO ()
solve possibleWords guesses results = do
    newResult <- getResult
    if newResult == "🟩🟩🟩🟩🟩" then do putStrLn "SOLVED!!!"
    else do
        let numberOfPossibleWords = length (getValidWords possibleWords guesses (results ++ [newResult]))
        let (word, entropy) = generateBestWord possibleWords guesses (results ++ [newResult])
        putStrLn ("There are now " ++ (show numberOfPossibleWords) ++ " possible words left")
        putStrLn ("Your next word: " ++ word ++ " with a score of " ++ (show entropy) ++ " bits")
        solve possibleWords (guesses ++ [word]) (results ++ [newResult])


main :: IO ()
main = do
    possibleWords <- readWords "data/top_400_words.txt"
    allowedWords <- readWords "data/allowed_words.txt"
    putStrLn "Enter a mode: either play or solve"
    mode <- getLineFixed

    if mode == "play" then do 
        targetWord <- getTargetWord possibleWords   
        putStrLn "Target word selected. Start guessing" 
        play targetWord 1 allowedWords

    else if mode == "solve" then do 
        putStrLn "Welcome to solve mode, use the following squares to enter the results ⬜🟨🟩"
        putStrLn "Start with the word crane"
        solve possibleWords ["crane"] []

    else if mode == "test" then do
        timeIt $ print (calculateEntropy "slate" [] [] possibleWords)
        timeIt $ putStrLn (show (fst (generateBestWord possibleWords ["crane", "split"] ["⬜⬜⬜⬜🟨", "⬜⬜🟨⬜⬜"])))
        putStrLn ("DEBUG ENTROPY CALCS: " ++ (show (calculateEntropy "slate" [] [] possibleWords)))

    else do
        putStrLn "Must choose either play or solve"
        main
    