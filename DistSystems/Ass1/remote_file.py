#!/usr/bin/env python
import base64
import json
import os
import random
import secure
import time
from services import lookup_as, lookup_ds, lookup_fs, get_ticket_for_file, hidden_file_path, unlock_file, patch_file

'''
This file declares the Remote File class.
It inherits from the local file class in python allowing it
to have all the read, write, readlines etc.
As it simply inherits, I get to choose what extra actions I want to overload.
On opening the file, it checks if file exists.
If it does, ping the server for changes and load the local file if it is the newest.
Otherwise get the file from the server.

On closing a file, in a modifiable mode ("a" or "w")
First we do a diff between cached version and new modified version (both local)
We then send a diff to the server of the changes that have been made.
The file is then closed.
'''

class RemoteFile(file):
	def __init__(self, path,mode='r', cached_directory="cached/", name="Richy", password="67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7"):
		#Rpath is the relative (virtual) filename
		self.rpath = path
		self.path = os.path.join(cached_directory, path);
		self._mode = mode
		self._name = name
		self.password = password
		self.session = None
		if os.path.exists(self.path) and not self.look_for_changes():
			file.__init__(self, self.path, mode)
		else:
			#Need to go to the remote.
			self.get_file_from_server()

	def look_for_changes(self):
		server_id, ticket, self.session = self.get_tickets()
		data = {
			"ticket": ticket,
			"request": {"type": "changed","message":  os.path.join(server_id[2], self.rpath) }
		}
		data["request"] = secure.encrypt_with_key(json.dumps(data["request"]), self.session)
		check = lookup_fs(data, self.path, server_id, self.session)
		#Some server socket error
		if(check == None):
			self.look_for_changes()
			return
		local_m_time = os.path.getmtime(self.path)
		if( local_m_time < float(check)):
			return True 
		else:
			return False

	def get_tickets(self):
		results = get_ticket_for_file(self.rpath, self._name, self.password, session=self.session)
		if(results == None):
			raise IOError("%s does not exist"%(self.path))
		tickets, session, servers_id = results
		#Load Balancing!
		server_choice = random.randint(0, len(servers_id)-1)
		server_id = servers_id[server_choice]
		ticket = tickets[server_choice]
		return (server_id, ticket, session)

	def get_file_from_server(self):
		server_id, ticket, self.session = self.get_tickets()
		data = {
			"ticket": ticket,
			"request": {"type": "open","relative": self.rpath,"message":  os.path.join(server_id[2], self.rpath) }
		}
		data["request"] = secure.encrypt_with_key(json.dumps(data["request"]), self.session)
		check = lookup_fs(data, self.path, server_id, self.session)
		if(check == None):
			self.get_file_from_server()
			return
		file.__init__(self, self.path, self._mode)
			
	def close(self):
		file.close(self)
		if not self._mode == 'r':
			command = "diff {0} {1} | tee {2} ".format(hidden_file_path(self.path), self.path, self.path+ ".diff")
			r= os.popen(command)
			server_id, ticket, self.session = self.get_tickets()
			diff_contents = r.read();
			data = {
				"ticket": ticket,
				"request": {
					"type":"write","message": os.path.join(server_id[2], self.rpath),
					"relative": self.rpath,"payload":base64.b64encode(diff_contents)
				}
			}
			data["request"] = secure.encrypt_with_key(json.dumps(data["request"]), self.session)
			result = lookup_fs(data, "", server_id, self.session)
			if result == None:
				raise IOError("Could not write to remote server: filename:%s"%(self.rpath))
			else:
				#Keeps mirrored local copy up to date.
				command = "cp {0} {1}".format(self.path, hidden_file_path(self.path))
				r = os.popen(command)
		unlock_file(self.rpath, self._name, self.password)


if __name__ == "__main__":
	rf = RemoteFile("Documents/big.txt", "r", "cached")
	print rf.read()
	rf.close()
	
