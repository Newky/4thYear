#!/usr/bin/env python
import SocketServer
import json
import random
import os

'''
The Authentication Server for the Distributed File System
Responsible for tickets and tokens
Using the algorithm described on the design brief.
'''

'''
Bit hacky but the only way to make a TCPServer 
reuse a port each time, otherwise locks port for a
set time each time.
'''
class TCPServer(SocketServer.TCPServer):
	allow_reuse_address = True

class RequestHandler(SocketServer.BaseRequestHandler):
    """
    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """
    def handle(self):
        # self.request is the TCP socket connected to the client
        self.data = self.request.recv(1024).strip()
	self.jdata = json.loads(self.data)
	if(self.jdata != None):
		if(self.jdata["type"]):
			if(self.jdata["type"] == "login"):
				self.handle_login()
	
    def handle_login(self):
	if(self.jdata["name"] and self.jdata["service"]):
		if self.jdata["name"] in names and self.jdata["service"] in services:
			#Handle successful login.
			#ticket consists of session key but is encrypted with server key	
			session_key = generate_session_key()
			ticket = [session_key]
			#Randomly choose a server
			server_id = random.choice(services[self.jdata["service"]])
			server_key = server_id[2]
			server_id = (server_id[0], server_id[1])	
			ticket = encrypt_with_key(ticket, server_key)
			jsonresult = {
				"ticket": "'"+ticket+"'",
				"session": session_key,
				"server_id": server_id
			}
			data = encrypt_with_key(json.dumps(jsonresult), names[self.jdata["name"]])
			self.request.send(data)
			return
	self.request.send("[]")

def encrypt_with_key(body, key):
	command = "echo \"%s\" | openssl enc -aes-256-cbc -a -salt -pass pass:%s" %(body, key)
	p = os.popen(command)
	data = p.read().strip()
	p.close()
	return data
	

def generate_session_key():
	alphabet = [ chr(i) for i in range(ord('a'), ord('z'))]
	str = ""
	for i in range(0, 6):
		str += random.choice(alphabet)
	return str



names = {
	"Richy" : "67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7", 
	"richdel": "67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7",
	"ec2-user": "67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7"
}

services = {
	"fs": [("localhost", 19999,"245ba14bbe3db735581b89871ec6d93cb95b058f")]
}

if __name__ == "__main__":
    HOST, PORT = "localhost", 9998
    #Had to overload TCPServer so it reuses the addr
    server = TCPServer((HOST, PORT), RequestHandler)
    #TCP server which serves forever on specified host and port.
    server.serve_forever()
