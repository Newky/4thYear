#!/usr/bin/env python
import json
import os
import secure
import SocketServer
'''
This is the password of the DS Server
This will be used to decrypt a client ticket received from the AS for the DS
Directories is a global config of a hash mapping of Directories to servers and
the location on servers of those file mappings.
'''
password = "a7899d63fcc914cf0dd008bf8ba6bb9f3bccd1ab"
directories = json.loads(open("config/ds.json", "r").read())

'''
Dir Lookup, takes a dir_name(this can be a dir name or a text_file)
Each Loop peels one sub-directory off and checks if this is in the hash map of
directory mappings

@params -> File Path

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
				return directories[dir_name][0]
			slash_index = dir_name.rfind("/")
		#Error State
		return []
			
'''
Wrapper around TCPServer class to make it reuse the port.
'''
class TCPServer(SocketServer.TCPServer):
	allow_reuse_address = True
'''
'''
class RequestHandler(SocketServer.BaseRequestHandler):
	def handle(self):
		self.data = self.request.recv(1024).strip()
		self.jdata = json.loads(self.data)
		ticket = json.loads(secure.decrypt_with_key(self.jdata["ticket"], password))
		message = secure.decrypt_with_key(self.jdata["message"], ticket[0]).strip()
		self.directory_data = dir_lookup(message)
		server_id = self.directory_data[:-1]
		server_ticket = self.directory_data[-1:][0]
		encrypted_ticket = secure.encrypt_with_key(json.dumps([ticket[0]]), server_ticket)
		client_msg = json.dumps(server_id)
		data = {
				"ticket":encrypted_ticket,
				"message":client_msg
		}
		print data
		#Encrypt response with session key
		print "Encrypting with {0}".format(ticket[0])
		self.request.send(secure.encrypt_with_key(json.dumps(data), ticket[0]))

if __name__ == "__main__":
	HOST, PORT = "localhost", 18888 
	#Had to overload TCPServer so it reuses the addr
	server = TCPServer((HOST, PORT), RequestHandler)
	#TCP server which serves forever on specified host and port.
	server.serve_forever()

	
