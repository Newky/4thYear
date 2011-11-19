#!/usr/bin/env python
import json
import os
import secure
import SocketServer

password = "a7899d63fcc914cf0dd008bf8ba6bb9f3bccd1ab"
directories = json.loads(open("config/ds.json", "r").read())

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
			

class TCPServer(SocketServer.TCPServer):
	allow_reuse_address = True

class RequestHandler(SocketServer.BaseRequestHandler):
	def handle(self):
		self.data = self.request.recv(1024).strip()
		self.jdata = json.loads(self.data)
		ticket = json.loads(secure.decrypt_with_key(self.jdata["ticket"], password))
		message = secure.decrypt_with_key(self.jdata["message"], ticket[0]).strip()
		client_msg = json.dumps(dir_lookup(message))
		#Encrypt response with session key
		self.request.send(secure.encrypt_with_key(client_msg, ticket[0]))

if __name__ == "__main__":
	HOST, PORT = "localhost", 18888 
	#Had to overload TCPServer so it reuses the addr
	server = TCPServer((HOST, PORT), RequestHandler)
	#TCP server which serves forever on specified host and port.
	server.serve_forever()

	
