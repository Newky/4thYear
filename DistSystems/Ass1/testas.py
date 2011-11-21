#!/usr/bin/env python
import base64
import json
import os
import secure
import socket
import sys

HOST, PORT = "localhost", 9998


def hidden_file_path(file_path):
	if file_path.rfind("/") == -1:
		return "." + file_path
	else:
		return file_path[0:file_path.rfind("/")+1] + "." + file_path[file_path.rfind("/")+1:] 

'''
Looks up authentication server
for a user.
Gets back [ticket, session key, [server address, server port]]
'''
def lookup_as(data, password):
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	received = None
	try:
		# Connect to server and send data
		sock.connect((HOST, PORT))
		sock.send(json.dumps(data))
		# Receive data from the server and shut down
		received = sock.recv(1024)
		received = secure.decrypt_with_key(received, password)
		received = json.loads(received)
	finally:
		sock.close()
		if(received):
			 return (received["ticket"],received["session"], received["server_id"])
		else:
			return None

def lookup_ds(message, server_id, ticket, session):
	data = {
		"message": secure.encrypt_with_key(message, session),
		"ticket": ticket
	}
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	try:
		sock.connect(( server_id[0], int(server_id[1]) ))
		sock.send(json.dumps(data))
		received = sock.recv(1024)
		received = secure.decrypt_with_key(received, session)
	finally:
		sock.close()
		if(received):
			return received
		else:
			return None
	
def lookup_fs(data, local_file, server_id, password):
	received = None
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	try:
		# Connect to server and send data
		sock.connect((    server_id[0], int(server_id[1])   ))
		sock.send(json.dumps(data))
		# Receive data from the server and shut down
		packet = None
		received="" 
		while 1:
			packet = sock.recv(1024)
			if not packet:  break
			received += packet
		received = secure.decrypt_with_key(received, password)
		received = json.loads(received)
	finally:
		sock.close()
		if received["type"] == "read":
			contents = ""
			if(received):
				contents = base64.b64decode(received["payload"])
			try:
				f = open(local_file, "wb")
				fc = open(hidden_file_path(local_file), "wb")
				f.write(contents)
				fc.write(contents)
				f.close()
				fc.close()
				return "Saved at {0}, cache saved at {1}".format(local_file, hidden_file_path(local_file))
			except OSError:
				return "Could not write to {0}".format(local_file)
		elif received["type"] == "write":
			return "Successful Write"
			
def get_ticket_for_file(file_to_lookup, name, password, session=None):
	if "ds" in aslookups:
		print "Using Cached AS lookup for DS"
		ticket, session, server_id = aslookups["ds"][0] 
	else:
		#Data to get ticket for DS from AS
		data = {
			"name": name,
			"type": "login", #What is going on here richy, why is it called login
			"service" : "ds"
		};
		results = lookup_as(data, password)
		aslookups["ds"] = [results]
		if results == None:
			return None
		ticket, session, server_id = results
	if  os.path.dirname(file_to_lookup) in dslookups:
		print "Using Cached DS lookup for "+file_to_lookup
		message = dslookups[os.path.dirname(file_to_lookup)]
	else:
		# Lookup the directory service for the file we are looking for
		message = lookup_ds(file_to_lookup, server_id, ticket, session)
		# json decode the message coming back
		# format (server address, server port, directory path(full))
		message= json.loads(message)
		dslookups[os.path.dirname(file_to_lookup)] = message
	if "fs" in aslookups:
		print "Using Cached AS lookup for FS"
		ticket, session, server_id = aslookups["fs"][0]
	else:
		# Prepare json request for FS to the AS given a server name we need a ticket for that server
		data = {
			"server": secure.encrypt_with_key(server_id[0], session), 
			"type": "login",
			"service": "fs" 
		}
		#Ticket, session, server_id from the as for the fs
		ticket, session, server_id = lookup_as(data, session)
		aslookups["fs"] = [(ticket, session, server_id)]

	return (ticket, session, server_id, message)


def file_open(file_to_lookup, local_file, name, password, session=None):
	results = get_ticket_for_file(file_to_lookup, name, password, session)
	if(results == None):
		return None
	ticket, session, server_id, message = results
	data = {
		"ticket": ticket,
		"message": {
			"type": "open",
			"message":  os.path.join(message[2], file_to_lookup)
		}
	}
	data["message"] = secure.encrypt_with_key(json.dumps(data["message"]), session)
	print lookup_fs(data, local_file, server_id, session)
	return session

def file_write(file_to_lookup, diff_file_name, name, password, session=None):
	results = get_ticket_for_file(file_to_lookup, name, password, session)
	if(results == None):
		return None
	ticket, session, server_id, message = results
	data = {
		"ticket": ticket,
		"message": {
			"type":"write",
			"message": os.path.join(message[2], file_to_lookup),
			"payload":base64.b64encode(open(diff_file_name, "rb").read())
		}
	}
	data["message"] = secure.encrypt_with_key(json.dumps(data["message"]), session)
	print lookup_fs(data, "", server_id, session)
	return session

dslookups = {}
aslookups = {}

if __name__ == "__main__":
	file_to_lookup = "Documents/Music/Test.txt"
	file_to_lookup2 = "Documents/Music/Test2.txt"
	file_to_lookup3 = "Documents/Music/Test3.txt"
	file_to_lookup4 = "Documents/Music/Test4.txt"
	password = "67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7"
	saved_location = "cached/test"
	session_key = file_open(file_to_lookup, saved_location, "Richy", password)
	session_key = file_open(file_to_lookup2, saved_location+"2", "Richy", password)
	session_key = file_open(file_to_lookup3, saved_location+"3", "Richy", password)
	session_key = file_open(file_to_lookup4, saved_location+"4", "Richy", password)
	os.popen("echo 'Test' >> {0}".format(saved_location) )
	command = "diff {0} {1} > {2} ".format(hidden_file_path(saved_location), saved_location, saved_location + ".diff")
	r= os.popen(command)
	session_key = file_write(file_to_lookup, saved_location + ".diff", "Richy", password, session_key)
	print session_key

