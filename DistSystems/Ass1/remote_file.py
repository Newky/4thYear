#!/usr/bin/env python
import base64
import json
import os
import random
import secure
import time
from services import lookup_as, lookup_ds, lookup_fs, get_ticket_for_file, hidden_file_path, unlock_file, patch_file

class RemoteFile(file):
	def __init__(self, path,mode='r', cached_directory="cached/", name="Richy", password="67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7"):
		self.rpath = path
		self.path = os.path.join(cached_directory, path);
		self._mode = mode
		self.local = None
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
		print "Looking for changes with session {0}".format(self.session)
		data = {
			"ticket": ticket,
			"request": {"type": "changed","message":  os.path.join(server_id[2], self.rpath) }
		}
		data["request"] = secure.encrypt_with_key(json.dumps(data["request"]), self.session)
		check = lookup_fs(data, self.path, server_id, self.session)
		local_m_time = os.path.getmtime(self.path)
		if( local_m_time < float(check)):
			print "Old file, getting new one."
			return True 
		else:
			return False

	def get_tickets(self):
		print "Getting tickets and session is:{0}".format(self.session) 
		print "Name:{0} Password:{1}".format(self._name,self.password)
		results = get_ticket_for_file(self.rpath, self._name, self.password, session=self.session)
		if(results == None):
			raise IOError("%s does not exist"%(self.path))
		#Split up the results tuple
		tickets, session, servers_id = results
		server_choice = random.randint(0, len(servers_id)-1)
		server_id = servers_id[server_choice]
		ticket = tickets[server_choice]
		return (server_id, ticket, session)

	def get_file_from_server(self):
		server_id, ticket, self.session = self.get_tickets()
		print "Getting file from server with session {0}".format(self.session)
		data = {
			"ticket": ticket,
			"request": {"type": "open","relative": self.rpath,"message":  os.path.join(server_id[2], self.rpath) }
		}
		data["request"] = secure.encrypt_with_key(json.dumps(data["request"]), self.session)
		check = lookup_fs(data, self.path, server_id, self.session)
		if(check == None):
			raise IOError("Could not retrieve %s"%(self.rpath))
		file.__init__(self, self.path, self._mode)
			
	def close(self):
		file.close(self)
		if not self._mode == 'r':
			command = "diff {0} {1} | tee {2} ".format(hidden_file_path(self.path), self.path, self.path+ ".diff")
			r= os.popen(command)
			server_id, ticket, self.session = self.get_tickets()
			print "Writing changes to server1 with session {0}".format(self.session)
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
				command = "cp {0} {1}".format(self.path, hidden_file_path(self.path))
				r = os.popen(command)
		unlock_file(self.rpath, self._name, self.password)


if __name__ == "__main__":
	rf = RemoteFile("Documents/Music/Test.txt", "a", "cached")
	print rf.write("Demo Write %s")
	raw_input("Press a key to close file");
	rf.close()
	
