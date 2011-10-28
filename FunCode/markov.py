#!/usr/bin/env python
import sys
import random

'''
This function builds a markov from a file object
It first reads the file line by line and splits them into words
It then fills a python dictionary using each word as a key and the
next word as a value. Because each word may have many entries, 
the value of the key in that dictionary is an array of all the words which
follow that word.

@params
filehandle - open file handle
'''
def build_markov(filehandle):
	wordlist = {}
	for line in filehandle.readlines():
		words = line.split(" ")
		for i in range(0, len(words)-1):
			try:
				wordlist[words[i].strip()].append(words[i+1].strip())
			except KeyError:
				wordlist[words[i].strip()] = [ words[i+1].strip() ]
	return wordlist

'''
Given a number of words, and a markov dictionary
Generate a story using the dictionary. First get a random word
from the dictionary, and then after that look up that word
entry in the dictionary for the next words, and repeat the
process for the number of words.
'''
def generate_markov(num, markov_dict):
	keys = markov_dict.keys()
	word = random.choice(keys)
	sys.stdout.write(word + " ")
	for i in range(0, num):
		try:
			words = markov_dict[word]
			word = random.choice(words)
		except KeyError:
			word = random.choice(keys)
		sys.stdout.write(word + " ")
	
'''
This is the main function
It takes the command line arguments
if a filename is given it opens it
otherwise it presumes stdin 
i.e piped file.
'''
def main():
	filehandle = None
	if len(sys.argv) > 1:
		filename = sys.argv[1]
		filehandle = open(filename, "r")
		num = int(raw_input("Number of words to generate:"));
	else:
		filehandle = sys.stdin
		num = 1000
	markov_dict = build_markov(filehandle)
	generate_markov(num, markov_dict)

if __name__ == "__main__":
	main()

