#!/usr/bin/env python
import os
import sys
import hashlib
from datetime import time
from remote_file import RemoteFile
'''
Test Handle for Demo
and example usage for the remote file class
'''

name = "Richy"
password = "67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7"

def read(name, password,filename = "Documents/Music/Test.hs", directory="cached"):
	rf = RemoteFile(filename, "r", directory, name, password)
	print rf.read()
	rf.close()
	return

def pause(name, password,filename = "Documents/Music/Test.hs", directory="cached"):
	rf = RemoteFile(filename, "r", directory, name, password)
	print rf.read()
	raw_input("Press enter to close file.")
	rf.close()
	return

def input_to_file(name, password,mode, filename = "Documents/Music/Test.hs", directory="cached"):
	rf = RemoteFile(filename, mode, directory, name, password)
	u_input = sys.stdin.readline()
	while(u_input != ".\n"):
		rf.write(u_input)
		u_input = sys.stdin.readline() 
	rf.close()
	return

def write(name, password,filename = "Documents/Music/Test.hs", directory="cached"):
	input_to_file( name, password,"w", filename, directory)
	return

def append(name, password,filename = "Documents/Music/Test.hs", directory="cached"):
	input_to_file(name, password,"a", filename, directory)
	return
	

if __name__ == "__main__":
	funcs = {
		"read": read,
		"write": write,
		"append":append,
		"pause":pause,
	}
	if len(sys.argv) > 1:
		if len(sys.argv) > 2:
			client= sys.argv[2]
			if len(sys.argv) > 4:
				name = sys.argv[3]
				password = hashlib.sha1(sys.argv[4]).hexdigest()
		else:
			client="cached"
			
		funcs[sys.argv[1]](name, password, directory=client)
	else:
		print "Need me some arguments"

