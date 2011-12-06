#!/usr/bin/env python
import base64
import os
import json
import socket
import secure

dslookups = {}
aslookups = {}

'''
Looks up authentication server
for a user.
Gets back [ticket, session key, [server address, server port]]
'''


def hidden_file_path(file_path):
	if file_path.rfind("/") == -1:
		return "." + file_path
	else:
		return file_path[0:file_path.rfind("/")+1] + "." + file_path[file_path.rfind("/")+1:] 


def lookup_as(data, password, HOST, PORT):
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
	received = None
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
	
def get_ticket_for_file(file_to_lookup, name, password, HOST="localhost", PORT=9998, session=None):
	global aslookups, dslookups
	if "ds" in aslookups:
		print "Using Cached AS lookup for DS"
		ticket, session, server_id = aslookups["ds"][0] 
	else:
		#Data to get ticket for DS from AS
		data = {
			"name": name,
			"type": "request",  
			"service" : "ds"
		};
		results = lookup_as(data, password, HOST, PORT)
		print results
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
		message= json.loads(message)
		dslookups[os.path.dirname(file_to_lookup)] = message
	#Ticket for FS from DS
	ticket = message["ticket"]
	server_id = json.loads(message["message"])

	return (ticket, session, server_id)

def lookup_fs(data, local_file, server_id, password):
	received = None
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	try:
		print "About to connect socket"
		# Connect to server and send data
		sock.connect((    server_id[0], int(server_id[1])   ))
		sock.send(json.dumps(data))
		# Receive data from the server and shut down
		packet = None
		received="" 
		print "socket connected, going into loop"
		while 1:
			packet = sock.recv(1024)
			if not packet:  break
			received += packet
		print "Here and now"
		received = secure.decrypt_with_key(received, password)
		received = json.loads(received)
	finally:
		sock.close()
		print "Received {0}".format(received)
		if received["type"] == "read":
			contents = ""
			if(received):
				contents = base64.b64decode(received["payload"])
			try:
				if not os.path.exists(os.path.dirname(local_file)):
					os.makedirs(os.path.dirname(local_file))
				f = open(local_file, "wb")
				fc = open(hidden_file_path(local_file), "wb")
				f.write(contents)
				fc.write(contents)
				f.close()
				fc.close()
				return "{0}".format(local_file)
			except OSError:
				return None
		elif received["type"] == "write":
			return "Success"
		else:
			return None

