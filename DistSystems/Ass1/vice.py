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
	if os.path.exists(path):
		with open(path, "rb") as handle:
			return xmlrpclib.Binary(handle.read())
	return xmlrpclib.Binary("");

def patch(name, path, lines, uniq_id):
	#global in_valid_files
	#try:
		#x = in_valid_files.index(path)
		#in_valid_files[path] = uniq_id
	#except KeyError:
		#in_valid_files[path] = uniq_id;
	#print in_valid_files
	print "{0} {1}".format(name, path)
	path = os.path.join("/home/"+name, path)
	diffpath = path + ".diff"
	f = open(diffpath, "wb")
	f.write(lines)
	f.close()
	print path
	if os.path.exists(path):
		f = os.popen('patch %s -i %s' %(path, diffpath))
		print f.read()
		return 0
	return 1

def valid(name, path, uniq_id, mtime):
	#global in_valid_files
	#print "Server Side: %s -> %s" %(",".join(in_valid_files), path)
	#try:
		#id = in_valid_files.get(path)
		#return (id == uniq_id) 
	#except KeyError:
		#return True
	try:
		server_mtime = os.path.getmtime(path)
		print "Server MTIME is %f" %(server_mtime)
		return (server_mtime < mtime)
	except OSError:
		return False;
def hello():
	print "Returned Hello"
	return "Hello"

users = {}
in_valid_files = {};

if __name__ == "__main__":
	if len(sys.argv) > 2:
		(HOST, PORT) = sys.argv[1:3]
		PORT = int(PORT)
	else:
		HOST = "localhost"
		PORT = 8080
	server = SimpleXMLRPCServer((HOST, PORT))
	functions = {
			"hello": hello,
			"lookup": lookup,
			"read": read_file,
			"insert": insert,
			"remove": remove,
			"patch" : patch,
			"valid" : valid
		}
	for k,v in functions.iteritems():
		server.register_function(v, k)
	server.serve_forever()

