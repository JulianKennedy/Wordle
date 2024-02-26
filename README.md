# WORDLE

# STEPS

- Calculate entropy
- Add word weights
- Possibly only use wordle word list

- Split the word into all GYWs
- Find all possible words that exist for each GYW and the true GYW or previous guesses
- Determine the weighting of the word based on the sum of all possible words from all GYWs
  - The highest weighting has the fewest possible words

Ideas

- Some kind of veritifcaton in solver to ensure result is all the proper boxes
