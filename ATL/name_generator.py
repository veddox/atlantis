'''
This script is meant to help me write "Crown of the Khadalun". It randomly
combines letters and syllables to suggest new fantasy names.
@author: Daniel Vedder
@version 1.1
'''

import random
import sys

global vowels, consonants
vowels = ("a", "e", "i", "o", "u")
consonants = ("b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z")

'''
Create a random syllable between 1 and 3 letters long
'''
def create_syllable():
    global vowels, consonants
    length = random.randint(1, 3)
    syllable = ""
    if length == 1:
        syllable = vowels[random.randint(0, 4)]
    else:
        for l in range(1, length+1, 1):
            next_letter = ""
            if l == 1 or l == 3:
                next_letter = consonants[random.randint(0, 20)]
            elif l == 2:
                next_letter = vowels[random.randint(0, 4)]
            syllable = syllable+next_letter
    return syllable

'''
Join several syllables together to form a word
@param length The number of syllables (0 for random up to 5)
'''
def create_word(length=0):
    if length == 0:
        length = random.randint(1, 5)
    word = ""
    for i in range(0, length+1, 1):
        next_syllable = create_syllable()
        word = word+next_syllable
    if word[-1] in vowels and random.randint(0, 1) == 1:
        word = word+consonants[random.randint(0, 20)]
    word = word[0].upper()+word[1:]
    return word

'''
Interactively suggest a list of names to the user, saving those
he/she chooses to file.
'''
def interactive():
    welcome_text = """
Welcome! This script will suggest a series of random names to 
you. After each name, type 'y' if you want to keep it, otherwise
type 'n' or just hit ENTER. To stop, type 'quit' or 'done'. 
'cancel' stops the session without saving anything."""
    print(welcome_text)
    names = ""
    next_name = create_word()
    keep = ""
    cancel = ("quit", "done", "exit", "cancel")
    while keep not in cancel:
        keep = input(">> "+next_name+": ")
        if keep == "y" or keep == "Y":
            names = names+next_name+"\n"
        next_name = create_word()
    if keep == "cancel":
        print("Script cancelled.")
        sys.exit()
    try:
        name_file = open("suggested_names.txt", "a")
        name_file.write(names)
        name_file.close()
    except:
        print("Error saving name list to suggested_names.txt!")
    print("Goodbye!")

'''
Print a list of random names
@param n Number of words, default: 10
'''
def batch(n=10):
    wordlist = create_word()
    for n in range(0, n-1, 1):
        wordlist = wordlist+", "+create_word()
    print(wordlist)

if __name__ == "__main__":
    if "-i" in sys.argv or "--interactive" in sys.argv:
        interactive()
    elif "-n" in sys.argv:
        n = sys.argv[sys.argv.index("-n")+1]
        batch(int(n))
    else:
        batch()
