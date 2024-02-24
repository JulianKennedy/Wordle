# Attribution: This project was created with the help of GitHub copilot

import random

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
        print("DEBUG BEFORE: ", numYellowToRemove, runningResult)
        runningResult = removeYellowIfNecessaryHelper(guess, runningResult, 4, char, numYellowToRemove)
        print("DEBUG AFTER: ", numYellowToRemove, runningResult)
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

# This should be within the main function
targetWord = getTargetWord()
print("Target word selected, start guessing")
play(1)
