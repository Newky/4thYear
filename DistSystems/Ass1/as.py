#!/usr/bin/env python
import json
import os
import random
import secure
import SocketServer
import time


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
users = {}

'''
Constant value for a user with no timeout
'''

NO_TIMEOUT = -1
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
			if(self.jdata["type"] == "request"):
				self.handle_ticket()
	
    def handle_ticket(self):
	if "service" in self.jdata:
		if "name" in self.jdata:
			print self.jdata
			if (self.jdata["name"] in names or is_server(self.jdata["name"])) and self.jdata["service"] in services:
				#Handle successful login.
				#ticket consists of session key but is encrypted with server key	
				print "Request from {0}".format(self.jdata["name"])
				session_key = generate_session_key()
				#Add the user and session key combination
				users[str(self.client_address[0])] = (session_key, NO_TIMEOUT)
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
				#if a known client
				if(self.jdata["name"] in names):
					data = secure.encrypt_with_key(json.dumps(jsonresult), names[self.jdata["name"]])
				else:
					#we have already checked that its a valid server.
					#Just need to extract its password from the config file.
					pwd = extract_password_from_server(self.jdata["name"])
					data = secure.encrypt_with_key(json.dumps(jsonresult), pwd)
				self.request.send(data)
				return
		elif self.jdata["service"] in services:
			if self.client_address[0] in users:
				if "server" in self.jdata:
					session_key = users[self.client_address[0]][0]
					server = secure.decrypt_with_key(self.jdata["server"], session_key).strip()
					servers_for_service = services[self.jdata["service"]]
					result = None 
					for x, y, z in servers_for_service:
						if x == server:
							result = (x, y, z)
							break
					if result != None:
						ticket = [session_key]
						server_id = result[0], result[1]
						server_key = result[2]
						ticket = secure.encrypt_with_key(json.dumps(ticket), server_key)
						jsonresult = {
							"ticket": ticket,
							"session": session_key,
							"server_id": server_id
						}
						data = secure.encrypt_with_key(json.dumps(jsonresult),session_key)
						self.request.send(data)
						return
			
	self.request.send("[]")


def generate_session_key():
	alphabet = [ chr(i) for i in range(ord('a'), ord('z'))]
	str = ""
	for i in range(0, 6):
		str += random.choice(alphabet)
	return str

#Already done the check below.
def extract_password_from_server(name):
	serv,cred  = name.split("/")
	host, port = cred.split(":")
	for h, p, pwd in services[serv]:
		if h == host and p == port:
			return pwd
	return None

# A server can query the as using
# name: service/HOST:PORT
def is_server(name):
	parts = name.split("/")
	if len(parts) <= 1:
		return False
	if parts[0] in services:
		try:
			shost, sport = parts[1].split(":")
		except ValueError:
			return False
		for host, port, _ in services[parts[0]]:
			if host == shost and port == sport:
				return True 
		return False

if __name__ == "__main__":
    HOST, PORT = "localhost", 9998
    #Had to overload TCPServer so it reuses the addr
    server = TCPServer((HOST, PORT), RequestHandler)
    print "Authentication Service running at {0}:{1}".format(HOST, PORT)
    #TCP server which serves forever on specified host and port.
    server.serve_forever()


