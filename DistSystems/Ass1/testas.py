#!/usr/bin/env python

import socket
import json
import sys
import os
import secure

HOST, PORT = "localhost", 9998
data = {
	"name": "Richy",
	"type": "login",
	"service": "ds"
}

password = "67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7"
# Create a socket (SOCK_STREAM means a TCP socket)
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

try:
	# Connect to server and send data
	sock.connect((HOST, PORT))
	sock.send(json.dumps(data))
	# Receive data from the server and shut down
	received = sock.recv(1024)
	received = secure.decrypt_with_key(received, password)
	print "Sent:     {0}".format(data)
	print "Received: {0}".format(received)
	received = json.loads(received)
	session_key = received["session"]
	data = {
		"message": secure.encrypt_with_key("Documents", session_key),
		"ticket" : received["ticket"]
	}
	sock.close()
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	print "Connecting to... {0}:{1}".format(received["server_id"][0], received["server_id"][1])
	sock.connect((received["server_id"][0], int(received["server_id"][1])))
	sock.send(json.dumps(data))
	received = sock.recv(1024)
	received = secure.decrypt_with_key(received, session_key)
	print received
	
finally:
    sock.close()

 
