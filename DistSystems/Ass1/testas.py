#!/usr/bin/env python
import base64
import json
import os
import re
import random
import secure
import socket
import sys
import time
from services import lookup_as, lookup_ds, lookup_fs, get_ticket_for_file, hidden_file_path


HOST, PORT = "localhost", 9998

def file_open(file_to_lookup,  name, password,local_file=None, session=None):
	results = get_ticket_for_file(file_to_lookup, name, password)
	if(results == None):
		return None
	tickets, session, servers_id = results
	server_choice = random.randint(0, len(servers_id)-1)
	server_id = servers_id[server_choice]
	ticket = tickets[server_choice]
	data = {
		"ticket": ticket,
		"request": {
			"type": "open",
			"message":  os.path.join(server_id[2], file_to_lookup)
		}
	}
	data["request"] = secure.encrypt_with_key(json.dumps(data["request"]), session)
	local_file = os.path.join("cached", file_to_lookup)
	try:
		os.makedirs(os.path.dirname(local_file))
	except OSError:
		pass
	print lookup_fs(data, local_file, server_id, session)
	return session

def file_write(file_to_lookup, diff_file_name, name, password, session=None):
	results = get_ticket_for_file(file_to_lookup, name, password)
	if(results == None):
		return None
	tickets, session, servers_id = results
	server_choice = random.randint(0, len(servers_id)-1)
	server_id = servers_id[server_choice]
	ticket = tickets[server_choice]
	contents = open(diff_file_name, "rb").read()
	data = {
		"ticket": ticket,
		"request": {
			"type":"write",
			"message": os.path.join(server_id[2], file_to_lookup),
			"relative": file_to_lookup,
			"payload":base64.b64encode(contents)
		}
	}
	data["request"] = secure.encrypt_with_key(json.dumps(data["request"]), session)
	print lookup_fs(data, "", server_id, session)
	return session

if __name__ == "__main__":
	name = "Richy"
	password = "67f8dc0c9f6451fb9e78ae43dafd2347caddf4a7"
	last_timestamped = time.time()
	line = sys.stdin.readline().strip()
	open_files = []
	while line != "/exit":
		r = re.match("^read\s(.*)$", line)
		if(r):
			if(r.group):
				file_to_lookup = r.group(1)
				print "Lookup "+file_to_lookup
				file_open(file_to_lookup, name, password)
				if not file_to_lookup in open_files:
					open_files.append((  os.path.join("cached", file_to_lookup), time.time() ))
			
		elif line == "sync":
			for _file, time_stamp in open_files:
				if os.path.getmtime(_file) > time_stamp:
					uncached = _file[_file.find("/")+1:]
					command = "diff {0} {1} > {2} ".format(hidden_file_path(_file), _file, _file+ ".diff")
					r= os.popen(command)
					command = "cp {0} {1}".format(_file, hidden_file_path(_file))
					r= os.popen(command)
					file_write(uncached, _file + ".diff", "Richy", password)
			last_timestamped = time.time()
		line = sys.stdin.readline().strip()

