#!/usr/bin/env python
import json
import os
import random
import secure
import SocketServer
import time

from services import check_servers_up, TaskThread
'''
The Authentication Server for the Distributed File System
Responsible for tickets and tokens
Using the algorithm described on the design brief.
'''

'''
Reads in the config file
Names is a list of users and their keys.
services is a list of services and their server details and secret keys.
oldservices is used when pinging the servers to see if they are down.
'''

config = json.loads(open("config/as.json", "r").read())
names = config["names"]
oldservices = dict(config["services"])
services = config["services"]
users = {}

'''
Need to overwrite base TCPServer to allow me 
to reuse the same address.
'''
class TCPServer(SocketServer.TCPServer):
	allow_reuse_address = True

class RequestHandler(SocketServer.BaseRequestHandler):
    """
    Request Handler instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """
    def handle(self):
        self.data = self.request.recv(1024).strip()
	#Each message is json format so important
	#to decode the json data first
	self.jdata = json.loads(self.data)
	if(self.jdata != None):
		if "type" in self.jdata:
			if(self.jdata["type"] == "ping"):
				self.request.send("[]")
			elif(self.jdata["type"] == "request"):
				self.handle_ticket()
	
    def handle_ticket(self):
	if "service" in self.jdata:
		if "name" in self.jdata:
			#Note the is_server part, if the request comes from one its own servers.
			if (self.jdata["name"] in names or is_server(self.jdata["name"])) and self.jdata["service"] in services:
				session_key = generate_session_key()
				#Cache the users ip address and session key.
				users[str(self.client_address[0])] = (session_key)
				ticket = [session_key]
				#Randomly choose a server for the request service
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

# This function is called at an interval.
# Checks servers are up.
def check_services():
	global oldservices, services
	print oldservices
	temp = dict(oldservices)
	for i in oldservices:
		temp[i] = oldservices[i][:]
	services = check_servers_up(temp)

if __name__ == "__main__":
    HOST, PORT = "localhost", 9998
    server = TCPServer((HOST, PORT), RequestHandler)
    print "Authentication Service running at {0}:{1}".format(HOST, PORT)
    Pinger = TaskThread()
    Pinger.task = check_services
    Pinger.start()
    server.serve_forever()


