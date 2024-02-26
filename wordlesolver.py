import json

# Step 1: Load the JSON file
with open('data/freq_map.json', 'r') as file:
    data = json.load(file)

# Step 2: Sort the words by their frequency in descending order
sorted_words = sorted(data.items(), key=lambda x: x[1], reverse=True)

# Step 3: Take the top 400 words
top_300_words = sorted([word for word, freq in sorted_words[:300]])

# Step 4: Write these words into a .txt file, separated by spaces
with open('data/top_300_words.txt', 'w') as file:
    file.write('\n'.join(top_300_words))
