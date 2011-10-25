#!/usr/bin/env python

import json

def read_in(user):
	path = "config/%s.json" %(user)
	try:
		f = open(path, "r")
		config = json.loads(f.read())
		f.close();
	except IOError:
		config = {};
	
	return config

if __name__ == "__main__":
	print read_in("richy")





