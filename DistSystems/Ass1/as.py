#!/usr/bin/env python
import SocketServer
import json
import random
import os
import secure


'''
The Authentication Server for the Distributed File System
Responsible for tickets and tokens
Using the algorithm described on the design brief.
'''

'''
Read in some of the config files
'''

config = json.loads(open("config/as.json", "r").read())
names = config["names"]
services = config["services"]

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
			ticket = secure.encrypt_with_key(json.dumps(ticket), server_key)
			jsonresult = {
				"ticket": ticket,
				"session": session_key,
				"server_id": server_id
			}
			data = secure.encrypt_with_key(json.dumps(jsonresult), names[self.jdata["name"]])
			self.request.send(data)
			return
	self.request.send("[]")

def generate_session_key():
	alphabet = [ chr(i) for i in range(ord('a'), ord('z'))]
	str = ""
	for i in range(0, 6):
		str += random.choice(alphabet)
	return str


if __name__ == "__main__":
    HOST, PORT = "localhost", 9998
    #Had to overload TCPServer so it reuses the addr
    server = TCPServer((HOST, PORT), RequestHandler)
    #TCP server which serves forever on specified host and port.
    server.serve_forever()


