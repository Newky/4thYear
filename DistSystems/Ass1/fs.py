#!/usr/bin/env python
import base64
import json
import os
import secure
import SocketServer

'''
File Service
============

ticket | message

The ticket contains the session key, which when decrypted with the server key reveals the session key
Message decrypted with session key.
Message is a json message of one of the following formats.

OPEN
{
	type: "open",
	message: "full/file/path/here"
}

What happens in the event of an open?
-------------------------------------

--> User + session key are cached. (is this needed?)
--> User (address, port) is added to a list of active for that file.

WRITE
{
	type: "write"
	message: "Diff of the changes"
}

What happens in the event of a write?
-------------------------------------

--> User will push the diff between old version and new version (this also shows that the user actually has the old file)
--> The server will update the server file.
--> Do something with all the clients which are active on that file. (some sort of ping) (Less important for now)

What happens if the file is not found on server?
------------------------------------------------

--> 
'''

password = "245ba14bbe3db735581b89871ec6d93cb95b058f"
class TCPServer(SocketServer.TCPServer):
	allow_reuse_address = True

class RequestHandler(SocketServer.BaseRequestHandler):
	def handle(self):
		self.data = self.request.recv(1024).strip()
		self.jdata = json.loads(self.data)
		ticket = secure.decrypt_with_key(self.jdata["ticket"], password)
		ticket = json.loads(ticket)
		message = secure.decrypt_with_key(self.jdata["message"], ticket[0])
		print message
		message = json.loads(message)
		if "type" in message:	
			if message["type"] == "open":
				self.handle_open(message, ticket[0])
			elif message["type"] == "write":
				self.handle_write(message, ticket[0])

	def handle_open(self, message, session_key):
		data = {
			"payload" : ""
		}
		try:
			file_name = message["message"]
			f = open(file_name, "rb")
			b64_text = base64.b64encode(f.read())
			data["payload"] = b64_text		
			f.close()
		except ValueError:
			print "No Message object on json message"
			data["error"] = "no message type"
		except OSError:	
			print "Error opening file"
			data["error"] = "io"
		finally:
			self.request.send(secure.encrypt_with_key(json.dumps(data), session_key))

	def handle_write(self, message):
		pass

if __name__ == "__main__":
	HOST, PORT = "localhost", 19999 
	#Had to overload TCPServer so it reuses the addr
	server = TCPServer((HOST, PORT), RequestHandler)
	#TCP server which serves forever on specified host and port.
	server.serve_forever()


