#!/usr/bin/env python
import base64
import json
import os
import secure
import SocketServer
import sys

from services import lookup_as, lookup_ds, lookup_fs, get_ticket_for_file

password = "245ba14bbe3db735581b89871ec6d93cb95b058f"
ASHOST = "localhost"
ASPORT = 9998

def patch_file(file_name, diff):
	try:
		f = open(file_name + ".diff", "w")
		f.write(diff)
		f.close()
		f = os.popen("patch %s -i %s" %(file_name, file_name + ".diff"))
		return f.read()
	except OSError:
		print "OS error opening {0}".format(file_name + ".diff")
		

class TCPServer(SocketServer.TCPServer):
	allow_reuse_address = True

class RequestHandler(SocketServer.BaseRequestHandler):
	def handle(self):
		self.data = self.request.recv(1024).strip()
		self.jdata = json.loads(self.data)
		ticket = secure.decrypt_with_key(self.jdata["ticket"], password)
		ticket = json.loads(ticket)
		print ticket
		message = secure.decrypt_with_key(self.jdata["request"], ticket[0])
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
			data["type"] = "read"
			self.request.send(secure.encrypt_with_key(json.dumps(data), session_key))

	def handle_write(self, message, session_key):
		data = {
			"status" : "Received Diff File"
		}
		try:
			file_name = message["message"]
			print "Writing changes to {0}".format(file_name)
			patch_output = patch_file(file_name, base64.b64decode(message["payload"]))
			print "Patch output:{0}".format(patch_output)
		except ValueError:
			print "Ugh message from user doesn't have the required fields"
			data["error"] = "Input"
		finally:
			data["type"] = "write"
			self.request.send(secure.encrypt_with_key(json.dumps(data), session_key))
			#This server is master, tell the slaves...
			if "relative" in message:
				self.replicate_changes(message["relative"], message["payload"])

	def replicate_changes(self, relative, body):
		print "I AM REPLICATING THE UPDATE"
		#Replicate changes. First must get ds entry from 
		name = "fs/{0}:{1}".format(HOST, PORT)
		print "Connection AS at {0}:{1}".format(ASHOST, ASPORT)
		results = get_ticket_for_file(relative, name, password)
		if(results == None):
			return None
		tickets, session, servers_id = results
		servers_id= [ [h, p, f] for h,p,f in servers_id if not (h == HOST and p == str(PORT))]
		print "Servers:{0}".format(servers_id)
		for i in range(0, len(servers_id)):
			#Note relative left out to let the file server know that
			#This is being replicated by a file server and doesnt need
			#further replication.
			ticket = tickets[i]
			server_id = servers_id[i]
			data = {
				"ticket": ticket,
				"request": {
					"type":"write",
					"message": os.path.join(server_id[2], relative),
					"payload":body
				}
			}
			print data
			data["request"] = secure.encrypt_with_key(json.dumps(data["request"]), session)
			print lookup_fs(data, "", server_id, session)


if __name__ == "__main__":
	if len(sys.argv) == 3:
		HOST, PORT = sys.argv[1], int(sys.argv[2])
	else:
		HOST, PORT = "localhost", 19999 
	print "File Service Started on {0}:{1}".format(HOST, PORT)
	#Had to overload TCPServer so it reuses the addr
	server = TCPServer((HOST, PORT), RequestHandler)
	#TCP server which serves forever on specified host and port.
	server.serve_forever()


