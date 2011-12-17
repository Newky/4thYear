#!/usr/bin/env python
import json
import os
import secure
import SocketServer
from services import check_servers_up, TaskThread
'''
Load in the json configuration file.
It consists of a hash map of directories.
'''
directories = json.loads(open("config/ds.json", "r").read())

'''
Dir Lookup, takes a dir_name(this can be a dir name or a text_file)
Each Loop peels one sub-directory off and checks if this is in the hash map of
directory mappings

Takes a file path

Returns something like this if successful:
	["localhost", "19999", "/home/richdel/dfs/"]
Otherwise it returns a []
'''
def dir_lookup(dir_name):
	if dir_name in directories:
		return directories[dir_name][0]
	else:
		slash_index = dir_name.rfind("/")
		while slash_index != -1:
			dir_name = dir_name[:slash_index]
			if dir_name in directories:
				return directories[dir_name]
				#return directories[dir_name][0]
			slash_index = dir_name.rfind("/")
		#Error State
		return []
			
'''
Wrapper around TCPServer class to make it reuse the port.
'''
class TCPServer(SocketServer.TCPServer):
	allow_reuse_address = True
'''
For the DS server, when message comes in, 
simply get the filename out of the message parameter.
do the directory lookup and return the relevant response
'''
class RequestHandler(SocketServer.BaseRequestHandler):
	def handle(self):
		self.data = self.request.recv(1024).strip()
		self.jdata = json.loads(self.data)
		#Ping check
		if "type" in self.jdata:
			if self.jdata["type"] == "ping":
				self.request.send("[]")
				return
		ticket = json.loads(secure.decrypt_with_key(self.jdata["ticket"], password))
		message = secure.decrypt_with_key(self.jdata["message"], ticket[0]).strip()
		#Note that there are all the file server which service that file
		#being returned, this allows the client to keep trying them until it finds
		#one which is live. It also allows the file server to push changes to each of the fileserver
		#when a change is detected.
		self.directory_data = dir_lookup(message)
		#Some weird list comprehensions, too much haskell on the brain.
		server_id = [ [host, port, file_name] for host, port, file_name, pwd in self.directory_data ]
		server_tickets = [ pwd for _,_,_,pwd in self.directory_data ]
		encrypted_ticket = [ secure.encrypt_with_key(json.dumps([ticket[0]]), pwd) for pwd in server_tickets ] 
		client_msg = json.dumps(server_id)
		data = {
				"ticket":encrypted_ticket,
				"message":client_msg
		}
		#Encrypt response with session key
		self.request.send(secure.encrypt_with_key(json.dumps(data), ticket[0]))

#This is called on an interval to
#Check if file servers are up
def check_services():
	global directories
	directories = check_servers_up(directories)

password = "a7899d63fcc914cf0dd008bf8ba6bb9f3bccd1ab"

if __name__ == "__main__":
	HOST, PORT = "localhost", 18888 
	#Had to overload TCPServer so it reuses the addr
	server = TCPServer((HOST, PORT), RequestHandler)
	print "Directory Service running at {0}:{1}".format(HOST, PORT)
	#TCP server which serves forever on specified host and port.
	Pinger = TaskThread()
	Pinger.task = check_services
	Pinger.start()
	server.serve_forever()

	
