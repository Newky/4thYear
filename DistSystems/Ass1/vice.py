#!/usr/bin/env python

import os
import sys
import xmlrpclib
from SimpleXMLRPCServer import SimpleXMLRPCServer

'''
Lookup
Insert
Delete
Administer
Read
Write
Lock
'''

'''
Flow of control is as follows
for read

User authenticates.
associate that file descriptor with the user's cached list
return binary file 

flow of control is follows as write 
User authenticates
Sends a diff of the file changes
server flushes differences to local file

'''

'''

also making the assumption that the user is allowed to access
only files in his home directory.

'''

'''
 If a path is a directory lookup will return a list of files and 
'''
def lookup(user_name, path):
	result = []
	if path[0] != ".":
		try:
			if os.path.isdir(path):
				for d in os.listdir(path):
					total = os.path.join(path, d)
					result.append( total )
			else:
				return ["file"] 
		except OSError:
			print "Ugh on "+path
			pass
	return result
'''
Insert a file name 
file name is a given path name and filename
All this does is effectively touch a file.
I assume that if someone wants to write a new
file, one needs to insert and then write.
0 for success
1 for non-success
'''
def insert(name, file_name, stats=None):
	try:
		open(file_name, "w").close()
		return 0
	except IOError:
		return 1

def remove(name, file_name):
	try:
		os.remove(file_name)
		return 0
	except IOError:
		return 1

def read_file(name, path):
	if name in users:
		users[name].append(path)
	else:
		users[name] = [path]
	if os.path.exists(path):
		with open(path, "rb") as handle:
			return xmlrpclib.Binary(handle.read())
	return xmlrpclib.Binary("");

def patch(name, path, lines):
	print "Lol"
	path = os.path.join("/home/"+name, path)
	print path
	if os.path.exists(path):
		f = os.popen('echo \'%s\' | patch %s' %(lines, path))
		print f.read()
		return 0
	return 1

users = {}

if __name__ == "__main__":
	HOST = "localhost"
	PORT = 8080
	server = SimpleXMLRPCServer((HOST, PORT))
	functions = {
			"lookup": lookup,
			"read": read_file,
			"insert": insert,
			"remove": remove,
			"patch" : patch
		}
	for k,v in functions.iteritems():
		server.register_function(v, k)
	server.serve_forever()
