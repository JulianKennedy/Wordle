# Attribution: This project was created with the help of GitHub copilot

import random
import math

list_of_allowed_words = set()
list_of_possible_words = []
targetWord = ""

with open('data/allowed_words.txt', 'r') as file:
    for line in file:
        list_of_allowed_words.add(line.strip())
        
with open('data/possible_words.txt', 'r') as file:
    for line in file:
        list_of_possible_words.append(line.strip())
#done        
def applyColorsGreedy(guess):
    result = ""
    for i in range(5):
        if guess[i] == targetWord[i]:
            result += "🟩"
        elif guess[i] in targetWord:
            result += "🟨"
        else:
            result += "⬜"
    return result
#done
def count(word, char):
    return word.count(char)
#done
def replace(word, index, newChar):
    wordAsList = list(word)
    wordAsList[index] = newChar
    return "".join(wordAsList)
#done
def removeYellowIfNecessaryHelper(guess, result, index, char, numYellowToRemove):
    if index >= 0 and guess[index] == char and numYellowToRemove > 0 and result[index] == "🟨":
        return removeYellowIfNecessaryHelper(guess, replace(result, index, "⬜"), index-1, char, numYellowToRemove-1)
    elif index >= 0:
        return removeYellowIfNecessaryHelper(guess, result, index-1, char, numYellowToRemove)
    else:
        return result
#done
def removeYellowIfNecessary(guess, result):
    runningResult = result
    for char in set(list(guess)):
        guess_freq = count(guess, char)
        target_freq = count(targetWord, char)
        numYellowToRemove = max(guess_freq - target_freq, 0)
        # print("DEBUG BEFORE: ", numYellowToRemove, runningResult)
        runningResult = removeYellowIfNecessaryHelper(guess, runningResult, 4, char, numYellowToRemove)
        # print("DEBUG AFTER: ", numYellowToRemove, runningResult)
    return runningResult
           
def displayFeedback(guess):
    initialResult = applyColorsGreedy(guess)
    filteredResult = removeYellowIfNecessary(guess, initialResult)   
    print(filteredResult)
#done
def getTargetWord():
    return list_of_possible_words[random.randint(0, len(list_of_possible_words) - 1)]
#done
def getGuess():
    guess = input()
    while guess not in list_of_allowed_words:
        print("GUESS AGAIN, that is not a word")
        guess = input()
    return guess

#done
def play(numGuesses):
    if numGuesses == 7:
        print(f"You are out of guesses. The correct word is {targetWord}")
        return
    guess = getGuess()
    isTargetWord = True if guess == targetWord else False
    if isTargetWord:
        print("🟩🟩🟩🟩🟩")
        print(f"You guessed the word in {numGuesses} guesses")
        print("CONGRATS MATE")
    else:
        displayFeedback(guess)
        play(numGuesses+1)



#############################################################
#############################################################
######################## SOLVE MODE #########################
#############################################################
#############################################################

#done       
def filterForContains(list_of_words, char, index):
    return [word for word in list_of_words if char in word and word[index] != char]

#done
def filterForNotContains(list_of_words, char):
    return [word for word in list_of_words if char not in word]

#done
def filterForPosition(list_of_words, char, index):
    return [word for word in list_of_words if word[index] == char]

#done
def filterFor(list_of_words, word, result, i):
    if result[i] == "⬜":
        return filterForNotContains(list_of_words, word[i])
    elif result[i] == "🟨":
        return filterForContains(list_of_words, word[i], i)
    elif result[i] == "🟩":
        return filterForPosition(list_of_words, word[i], i)

#done
def filter(list_of_words, word, result):

    possibleWords = list_of_words.copy()
    for i in range(5):
        possibleWords = filterFor(possibleWords, word, result, i)

    return possibleWords
    
#done
def getValidWords(words, results):
    
    possibleWords = list_of_possible_words.copy()
    for word, result in zip(words, results):
        possibleWords = filter(possibleWords, word, result)
    
    return possibleWords
    
#done
# p * log_2(1/p) 
#p = # of words in a partition
def calculateEntropy(word, guesses, results):
    # print("DEBUG calculateEntropy: "+)
    runningTotal = 0
    partitionList = generateResultComb(0,[""])

    denominator = len(getValidWords(guesses, results))
    
    print("DEBUG calculateEntropy denominator: ", denominator)
    
    for partition in partitionList:
        validWordCount = len(getValidWords(guesses + [word], results + [partition]))
        p = validWordCount/denominator
        if p > 0:
            entropy = p * math.log(1/p, 2)
            runningTotal += entropy
    
    print("DEBUG calculateEntropy: ", word, runningTotal)
    return runningTotal

#done
# initialize as (0, [""])
def generateResultComb(size, currentResults):
    if size == 5:
        return currentResults

    addWhite = list(map(lambda s: s+'⬜', currentResults))
    addYellow = list(map(lambda s: s+'🟨', currentResults))
    addGreen = list(map(lambda s: s+'🟩', currentResults))

    return generateResultComb(size+1, addWhite + addYellow + addGreen)

#done
def getWordWithHighestEntropy(values, words, highestValue, highestValueWord):
    # if not len(values):
    #     return highestValueWord
        
    # if values[0] > highestValue:
    #     return getWordWithHighestEntropy(values[1:], words[1:], values[0], words[0])
    # else:    
    #     return getWordWithHighestEntropy(values[1:], words[1:], highestValue, highestValueWord)
    for value, word in zip(values, words):
        if value > highestValue:
            highestValue = value
            highestValueWord = word
    return highestValueWord
        
#done
def generateBestWord(guesses, results):
    if len(getValidWords(guesses, results)) <= 2:
        return getValidWords(guesses, results)[0], calculateEntropy(getValidWords(guesses, results)[0], guesses, results)
    entropyValues = [calculateEntropy(word, guesses, results) for word in list_of_possible_words]
    return getWordWithHighestEntropy(entropyValues, list_of_possible_words, 0, "aaaaa"), max(entropyValues)

def solve():
    print("Welcome to solve mode. Give results in ⬜🟨🟩")       
    # Based on science (3Blue1Brown video)
    print("Start with the word crane")
    words = ["crane"]
    results = []
    # while len(results) == 0 or results[-1] != "🟩🟩🟩🟩🟩":
    while True:
        newResult = input("Enter result: ")
        results.append(newResult)
        if newResult == "🟩🟩🟩🟩🟩":
            print("SOLVED!")
            break
        
        print(f"There are now {len(getValidWords(words, results))} possible words left")
        nextWord, numBits = generateBestWord(words, results)
        words.append(nextWord)
        print(f"Your next word: {nextWord} with a score of {numBits} bits")
    
print("DEBUG ENTROPY CALCS: ", calculateEntropy("slate", [], []))
print("DEBUG getValidWord: ", getValidWords(["crane", "split"], ["⬜⬜⬜⬜🟨", "⬜⬜🟨⬜⬜"]))
print("DENIG: ", len(getValidWords(["crane"], ["⬜⬜⬜⬜⬜"])))
print("DEBUG getValidWord: ", len(getValidWords(["crane"], ["⬜⬜⬜⬜🟨"])))

# This should be within the main function
targetWord = getTargetWord()
print("Pick a mode: play or solve")
mode = input()
while mode != "play" and mode != "solve":
    print("Must choose either play or solve")
    mode = input()

if mode == "play":
    print("Target word selected, start guessing")
    play(1)
elif mode == "solve":
    solve()