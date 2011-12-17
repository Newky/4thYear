#!/usr/bin/env python
import json
import secure
import SocketServer
import sys


class TCPServer(SocketServer.TCPServer):
	allow_reuse_address = True

'''
Data will be sent to lock service as follows:

	{
		"ticket" : [session],
		"filename" : "relative filename here"
		"type": "unlock/lock"
	}

the ticket will be encrypted with the servers password.
the filename will be encrypted with the session key.

Locked files are stored as a hash of
users ip address and their names.
This is so two computers which come from same IP address can't unlock files the files which the other
unlocked. I.e two clients behind one router.
'''
class RequestHandler(SocketServer.BaseRequestHandler):
	def handle(self):
		resp = {"status_code" : 0}
		self.data = self.request.recv(1024).strip()
		try:
			self.jdata = json.loads(self.data)
			#This ping message is for servers trying to
			#make sure its live.
			if "type" in self.jdata:
				if self.jdata["type"] == "ping":
					self.request.send("[]")
					session = None
					return
			session = secure.decrypt_with_key(self.jdata["ticket"], password).strip()
			session = json.loads(session)[0]
			filename = secure.decrypt_with_key(self.jdata["filename"], session).strip()
			filename = secure.decrypt_with_key(self.jdata["filename"], session).strip()
			if "type" in self.jdata:
				if self.jdata["type"] == "ping":
					self.request.send("[]")
					return
				elif self.jdata["type"] == "unlock":
					if filename in locked_files:
						if locked_files[filename] == str(self.client_address[0]) + self.jdata["name"]:
							del locked_files[filename]
							resp["message"] = "Successfully unlocked {0}".format(filename)
							resp["status_code"] = 1
						else:
							resp["message"] = "You don't have the correct permission to unlock this file."
					else:
						resp["message"] = "File is not locked so an unlock is not possible."
				elif self.jdata["type"] == "lock":
					if filename in locked_files:
						if locked_files[filename] == str(self.client_address[0]) + self.jdata["name"]:
							resp["message"] = "You already have the lock for {0}".format(filename)
							resp["status_code"] = 1
						else:
							resp["message"] = "Filename {0} is locked. Unable to access.".format(filename)	
					else:
						locked_files[filename] = str(self.client_address[0]) + self.jdata["name"]
						resp["message"] = "Filename {0} successfully locked.".format(filename)
						resp["status_code"] = 1
			else:
				resp["message"] = "Incorrect message parameters."
		except ValueError:
			resp["message"] = "Incorrect incoming json message."
		except KeyError:
			resp["message"] = "Incorrect incoming json parameters."
		finally:
			if session:
				jdata= secure.encrypt_with_key(json.dumps(resp), session)
			else:
				jdata = json.dumps(resp)
			self.request.send(jdata)

locked_files = {}
password = "67f8dc0c9f6451fb9e78ae43dafd2347csubf4a7"

if __name__ == "__main__":
	if len(sys.argv) == 3:
		HOST, PORT = sys.argv[1], int(sys.argv[2])
	else:
		HOST, PORT = "localhost", 17777
	print "Lock Service Started on {0}:{1}".format(HOST, PORT)
	#Had to overload TCPServer so it reuses the addr
	server = TCPServer((HOST, PORT), RequestHandler)
	#TCP server which serves forever on specified host and port.
	server.serve_forever()


