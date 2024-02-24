list_of_possible_words = []

with open('data/possible_words.txt', 'r') as file:
    for line in file:
        list_of_possible_words.append(line.strip())

contains = "rae"
not_contains = "plutocnes"

for word in list_of_possible_words:
    if all([char in word for char in contains]) and all([char not in word for char in not_contains]):
        print(word)