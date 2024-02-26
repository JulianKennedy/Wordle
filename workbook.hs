filterForContains :: [String] -> Char -> Int -> [String]
filterForContains low char index = [word | word <- low, char `elem` word && word !! index /= char]

filterForNotContains :: [String] -> Char -> [String]
filterForNotContains low char = [word | word <- low, char `notElem` word]

filterForPosition :: [String] -> Char -> Int -> [String]
filterForPosition low char index = [word | word <- low, word !! index == char]

filterFor :: [String] -> String -> String -> Int -> [String]
filterFor low word result index = 
    if (result !! index) == '⬜' then filterForNotContains low (word !! index)
    else if result !! index == '🟨' then filterForContains low (word !! index) index
    else filterForPosition low (word !! index) index

filterr :: [String] -> String -> String -> [String]
filterr low word result = foldl (\acc i -> filterFor acc word result i) low [0..4]

getValidWords :: [String] -> [String] -> [String] -> [String]
getValidWords low words results = foldl (\acc i -> filterr acc (words !! i) (results !! i)) low [0..((length words) - 1)]








-- Reads a list of words from a file
readWords :: FilePath -> IO [String]
readWords path = do
    contents <- readFile path
    return $ lines contents

main :: IO ()
main = do
    possibleWords <- readWords "data/possible_words.txt"
    allowedWords <- readWords "data/allowed_words.txt"
    putStrLn "EXPECTED 1"
    putStrLn (show (length (getValidWords possibleWords ["crane"] ["🟩🟩🟩🟩🟩"])))
    putStrLn "EXPECTED 263"
    putStrLn (show (length (getValidWords possibleWords ["crane"] ["⬜⬜⬜⬜⬜"])))
    putStrLn "EXPECTED 16"
    putStrLn (show (length (getValidWords possibleWords ["crane", "split"] ["⬜⬜⬜⬜🟨", "⬜⬜🟨⬜⬜"])))