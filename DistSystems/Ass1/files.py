#!/usr/bin/env python
import os

def ls(path):
	try:
		result = []
		for dirname, dirnames, filenames in os.walk(path):
			result = [dirnames, filenames]
			break
	except OSError:
		return []
	#for i in range(0, len(result[0])):
		#result[0][i] = path + result[0][i]
	return result 

def root_dir(path):
	return path[: path.index("/")+1]

