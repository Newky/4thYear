#!/usr/bin/env python

import socket
import json
import sys
import os

HOST, PORT = "localhost", 9998
data = {
	"name": "Richy",
	"type": "login",
	"service": "fs"
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
finally:
    sock.close()
 
command = "echo \"%s\" | openssl enc -d -aes-256-cbc -a -pass pass:%s" %(received, password)
p = os.popen(command)
received = p.read();
p.close()
print "Sent:     {0}".format(data)
print "Received: {0}".format(received)
