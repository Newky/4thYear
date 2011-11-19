#!/usr/bin/env python
import base64
import socket
import json
import sys
import os
import secure

HOST, PORT = "localhost", 9998

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
	


if __name__ == "__main__":
	file_to_lookup = "Documents/Music/Test.txt"
	password = "67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7"
	data = {
		"name": "Richy",
		"type": "login",
		"service": "ds" 
	}
	results = lookup_as(data, password)
	if(results):
		ticket, session, server_id = results
		message = lookup_ds(file_to_lookup, server_id, ticket, session)
		message= json.loads(message)
		print "Encrypting using {0}".format(session)
		data = {
			"server": secure.encrypt_with_key(server_id[0], session), 
			"type": "login",
			"service": "fs" 
		}
		results = lookup_as(data, session)
		ticket, session, server_id = results
		data = {
			"ticket": ticket,
			"message": {
				"type": "open",
				"message":  os.path.join(message[2], file_to_lookup)
			}
		}
		print "Encrypting using {0}".format(session)
		data["message"] = secure.encrypt_with_key(json.dumps(data["message"]), session)
		received = None
		sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		try:
			# Connect to server and send data
			sock.connect((    server_id[0], int(server_id[1])   ))
			sock.send(json.dumps(data))
			# Receive data from the server and shut down
			received = sock.recv(1024)
			received = secure.decrypt_with_key(received, session)
			received = json.loads(received)
		finally:
			sock.close()
			if(received):
				print base64.b64decode(received["payload"])
			else:
				print None
			

