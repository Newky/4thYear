#!/usr/bin/env python
import base64
import json
import os
import random
import secure
import time
from services import lookup_as, lookup_ds, lookup_fs, get_ticket_for_file, hidden_file_path, unlock_file

class RemoteFile(file):
	def __init__(self, path,mode='r', cached_directory="cached/"):
		self.rpath = path
		self.path = os.path.join(cached_directory, path);
		self._mode = mode
		self.local = None
		if os.path.exists(self.path) and not self.ping_for_changes():
			file.__init__(self, self.path, mode)
		else:
			#Need to go to the remote.
			self.get_file_from_server()

	def ping_for_changes(self):
		server_id, ticket, session = self.get_tickets()
		data = {
			"ticket": ticket,
			"request": {"type": "ping","message":  os.path.join(server_id[2], self.rpath) }
		}
		data["request"] = secure.encrypt_with_key(json.dumps(data["request"]), session)
		check = lookup_fs(data, self.path, server_id, session)
		local_m_time = os.path.getmtime(self.path)
		if( local_m_time < float(check)):
			print "Old file, getting new one."
			return True 
		else:
			return False

	def get_tickets(self):
		results = get_ticket_for_file(self.rpath, name, password)
		if(results == None):
			raise IOError("%s does not exist"%(self.path))
		print results
		#Split up the results tuple
		tickets, session, servers_id = results
		server_choice = random.randint(0, len(servers_id)-1)
		server_id = servers_id[server_choice]
		ticket = tickets[server_choice]
		print "Options "+str((server_choice, server_id, ticket, session))
		return (server_id, ticket, session)

	def get_file_from_server(self):
		server_id, ticket, session = self.get_tickets()
		data = {
			"ticket": ticket,
			"request": {"type": "open","relative": self.rpath,"message":  os.path.join(server_id[2], self.rpath) }
		}
		data["request"] = secure.encrypt_with_key(json.dumps(data["request"]), session)
		check = lookup_fs(data, self.path, server_id, session)
		if(check == None):
			raise IOError("Could not retrieve %s"%(self.rpath))
		file.__init__(self, self.path, self._mode)
			
	def close(self):
		file.close(self)
		if not self._mode == 'r':
			command = "diff {0} {1} | tee {2} ".format(hidden_file_path(self.path), self.path, self.path+ ".diff")
			r= os.popen(command)
			server_id, ticket, session = self.get_tickets()
			diff_contents = r.read();
			data = {
				"ticket": ticket,
				"request": {
					"type":"write","message": os.path.join(server_id[2], self.rpath),
					"relative": self.rpath,"payload":base64.b64encode(diff_contents)
				}
			}
			data["request"] = secure.encrypt_with_key(json.dumps(data["request"]), session)
			result = lookup_fs(data, "", server_id, session)
			if result == None:
				raise IOError("Could not write to remote server: filename:%s"%(self.rpath))
		unlock_file(self.rpath, name, password)

name = "Richy"
password = "67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7"
if __name__ == "__main__":
	rf = RemoteFile("Documents/Music/Test.txt", "r", "cached")
	print rf.readlines()
	raw_input("Press a key to close file");
	rf.close()
	#time.sleep(5)
	#rf = RemoteFile("Documents/strings", "w")
	#rf.write('''
#I, man, am regal - a German am I
#Never odd or even
#If I had a hifi
#Madam, I'm Adam
#Too hot to hoot
#No lemons, no melon
#Too bad I hid a boot
#Lisa Bonet ate no basil
#Warsaw was raw
#Was it a car or a cat I saw?

#Rise to vote, sir
#Do geese see God?
#"Do nine men interpret?" "Nine men," I nod
#Rats live on no evil star
#Won't lovers revolt now?
#Race fast, safe car
#Pa's a sap
#Ma is as selfless as I am
#May a moody baby doom a yam?

#Ah Satan sees Natasha
#No devil lived on
#Lonely Tylenol 
#Not a banana baton
#No "x" in "Nixon"
#O, stone, be not so
#O Geronimo, no minor ego
#"Naomi", I moan
#"A Toyota's a Toyota"
#A dog, a panic in a pagoda

#Oh, no! Don Ho!
#Nurse, I spy gypsies -- run!
#Senile felines
#Now I see bees I won
#UFO tofu
#We panic in a pew
#Oozy rat in a sanitary zoo
#God! A red nugget! A fat egg under a dog!
#Go hang a salami, I'm a lasagna hog
	#''');
	#rf.close();

