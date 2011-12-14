#!/usr/bin/env python
import os
import sys
from datetime import time
from remote_file import RemoteFile
name = "Richy"
password = "67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7"

def read(name, password,filename = "Documents/Music/Test.txt", directory="cached"):
	rf = RemoteFile(filename, "r", directory, name, password)
	print rf.read()
	rf.close()
	return

def pause(name, password,filename = "Documents/Music/Test.txt", directory="cached"):
	rf = RemoteFile(filename, "r", directory, name, password)
	print rf.read()
	raw_input("Press enter to close file.")
	rf.close()
	return


def date_to_file(name, password,mode, filename = "Documents/Music/Test.txt", directory="cached"):
	rf = RemoteFile(filename, mode, directory, name, password)
	rf.write(os.popen("date").read())
	rf.close()
	return

def write(name, password,filename = "Documents/Music/Test.txt", directory="cached"):
	date_to_file("w", filename, directory, name, password)
	return

def append(name, password,filename = "Documents/Music/Test.txt", directory="cached"):
	date_to_file("a", filename, directory, name, password)
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
				password = sys.argv[4]
		else:
			client="cached"
			
		funcs[sys.argv[1]](name, password, directory=client)
	else:
		print "Need me some arguments"

