#!/usr/bin/env python
import base64
import os
import json
import socket
import secure
import sys
import threading

#These are caches for lookups
#Having them here means that both the servers
#and the client can benefit from these.
#By that I don't mean they share caches but more
#that they can both import this module
dslookups = {}
aslookups = {}
TIMEOUT = 5.0

#Given a file path create a hidden one (unix def of hidden)
#i.e /Documents/test.txt -> /Documents/.test.txt
#Used mainly client side for creating the original mirror of
#the opened file.
def hidden_file_path(file_path):
	if file_path.rfind("/") == -1:
		return "." + file_path
	else:
		return file_path[0:file_path.rfind("/")+1] + "." + file_path[file_path.rfind("/")+1:] 

'''
This function looks up the as.
Parameters are:
Data -> some dictionary with the request body.
Password -> This is just a session code.
Host -> host of as
Port -> port of as
The host and port of the as are assumed known in my system.
'''
def lookup_as(data, password, HOST, PORT):
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	received = None
	try:
		# Connect to server and send data
		sock.connect((HOST, PORT))
		sock.send(json.dumps(data))
		# Receive data from the server and shut down
		received = sock.recv(1024)
		received = secure.decrypt_with_key(received, password)
		received = json.loads(received)
	finally:
		sock.close()
		if(received):
			 return (received["ticket"],received["session"], received["server_id"])
		else:
			return None

'''
Looks up the ls
Builds the message body internally.
filename -> this is a virtual filename.
request -> lock/unlock
server_id -> this is host, port details of ls server.
ticket -> ticket got from AS for LS
session key
name -> the users name.
'''
def lookup_ls(filename, request, server_id, ticket, session, name):
	data = {
		"ticket": ticket,
		"filename": secure.encrypt_with_key(filename, session),
		"type":request,
		"name": name
	}
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	received = None
	try:
		sock.connect(( server_id[0], int(server_id[1]) ))
		sock.send(json.dumps(data))
		received = sock.recv(1024)
		received = secure.decrypt_with_key(received, session)
	finally:
		sock.close()
		if(received):
			#print received
			return json.loads(received)
		else:
			return None
'''
Ds Lookup function.
message -> filename to be looked up.
server_id -> host, port tuple of the DS server
ticket -> ticket from AS for DS
session key
'''
def lookup_ds(message, server_id, ticket, session):
	data = {
		"message": secure.encrypt_with_key(message, session),
		"ticket": ticket
	}
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	received = None
	try:
		sock.connect(( server_id[0], int(server_id[1]) ))
		sock.send(json.dumps(data))
		received = sock.recv(1024)
		received = secure.decrypt_with_key(received, session)
	finally:
		sock.close()
		if(received):
			return received
		else:
			return None
'''
A function to unlock the file on the ls.
Makes sure the file is unlockable.
file_to_lookup -> file name (virtual)
name -> users name
session key
HOST and PORT of as.
'''
def unlock_file(file_to_lookup, name, session, HOST="localhost", PORT=9998):
	global aslookups
	# AS LOOKUP
	# FOR LS
	ticket, session, server_id = check_as_cache(name, session, "ls", HOST, PORT) 
	ls_results = lookup_ls(file_to_lookup, "unlock", server_id, ticket, session, name)
	if "status_code" in ls_results:
		if ls_results["status_code"] == 0:
			raise IOError(ls_results["message"])
	else:
		return None

'''
Check if an as lookup is cached for that service
If not do one and cache it.
name -> User's name
session key
service -> Name of service.
HOST and PORT of AS
'''
def check_as_cache(name,session,service, HOST, PORT):
	global aslookups
	if service in aslookups:
		return aslookups[service][0] 
	else:
		data = {
			"name": name,
			"type": "request",  
			"service" : service 
		};
		results = lookup_as(data, password, HOST, PORT)
		aslookups[service] = [results]
		if results == None:
			return None
		return results

'''
Idea of this is to get the necessary file server ticket
for a file. It does the LS lookup if needed (not needed if your a file server)
Does DS lookup. as well as AS lookups for each of those services.
It then returns the results of the DS lookup which on a successful request
should return a ticket, session code and server id of the file server
which has the file to lookup.
file_to_lookup -> virtual filename
name -> User's name
password -> session_key
HOST and PORT of AS

'''
def get_ticket_for_file(file_to_lookup, name, password, HOST="localhost", PORT=9998, session=None, ls_needed=True):
	global aslookups, dslookups
	# AS LOOKUP
	# FOR LS
	if(ls_needed):
		ticket, session, server_id= check_as_cache(name, password, "ls", HOST, PORT) 
		ls_results = lookup_ls(file_to_lookup, "lock", server_id, ticket, session, name)
		if "status_code" in ls_results:
			if ls_results["status_code"] == 0:
				raise IOError(ls_results["message"]);
		else:
			return None
	# AS LOOKUP
	# FOR DS
	ticket, session, server_id = check_as_cache(name,password,"ds", HOST, PORT) 
	# DS LOOKUP
	# FOR THE DESIRED FILE.
	if  os.path.dirname(file_to_lookup) in dslookups:
		message = dslookups[os.path.dirname(file_to_lookup)]
	else:
		# Lookup the directory service for the file we are looking for
		message = lookup_ds(file_to_lookup, server_id, ticket, session)
		# json decode the message coming back
		message= json.loads(message)
		dslookups[os.path.dirname(file_to_lookup)] = message
	#Ticket for FS from DS
	ticket = message["ticket"]
	server_id = json.loads(message["message"])

	return (ticket, session, server_id)

'''
Looks up the file system given some dictionary as a request.
If there is an error in the connect, it invalidates the caches (empties them)
and returns None
local_file -> filename.
server_id -> details of File Server
password -> session key
'''
def lookup_fs(data, local_file, server_id, password):
	received = None
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	try:
		# Connect to server and send data
		sock.connect((    server_id[0], int(server_id[1])   ))
		content = json.dumps(data)
		content_length = json.dumps({ 'content-length': len(content)})
		sock.send(content_length)
		received = sock.recv(10)
		sock.send(content)
		# Receive data from the server and shut down
		packet = None
		received="" 
		while 1:
			packet = sock.recv(1024)
			if not packet:  break
			received += packet
		received=secure.decrypt_with_key(received, password)
		received=json.loads(received)
	except:
		sock.close()
		aslookups = {}
		dslookups = {}
	finally:
		sock.close()
		if "type" in received:
			if received["type"] == "read":
				contents = ""
				if(received):
					contents = base64.b64decode(received["payload"])
				try:
					if not os.path.exists(os.path.dirname(local_file)):
						os.makedirs(os.path.dirname(local_file))
					f = open(local_file, "wb")
					fc = open(hidden_file_path(local_file), "wb")
					f.write(contents)
					fc.write(contents)
					f.close()
					fc.close()
					return "{0}".format(local_file)
				except OSError:
					return None
			elif received["type"] == "write":
				return "Success"
			elif received["type"] == "ping":
				if "error" in received:
					return received["error"]
				else:
					return received["mtime"]
		return None

'''
A function which given a hash map of services.
in the form 

{
	"ds" : [
		[host, port]
	]
	...

}
'''
def check_servers_up(services):
	for serv in services:
		service = services[serv]
		for i in range(0, len(service)):
			inst = service[i]
			host = inst[0]
			port = inst[1]
			try:
				sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
				sock.settimeout(TIMEOUT)
				sock.connect((host, int(port)))
				sock.send(json.dumps({"type":"ping"}))
				received = sock.recv(1024)
			except:
				print "{2} at {0}:{1} is down".format(inst[0], inst[1], serv)
				service[i] = None
			finally:
				sock.close()
		#Any server with a connection error gets None as its value.
		#Filter them out.
		services[serv] = filter((lambda x: x!= None), service)
	return services

class TaskThread(threading.Thread):
    """Thread that executes a task every N seconds"""
    
    def __init__(self):
        threading.Thread.__init__(self)
	threading.Thread.daemon = True
        self._finished = threading.Event()
        self._interval = 20.0
    
    def setInterval(self, interval):
        """Set the number of seconds we sleep between executing our task"""
        self._interval = interval
    
    def shutdown(self):
        """Stop this thread"""
        self._finished.set()
    
    def run(self):
        while 1:
            if self._finished.isSet(): return
            self.task()
            
            # sleep for interval or until shutdown
            self._finished.wait(self._interval)
    
    def task(self):
        """The task done by this thread - override in subclasses"""
        pass

'''
Given filename and a payload of a diff file.
patch it locally with that diff.
'''
def patch_file(file_name, diff):
	try:
		f = open(file_name + ".diff", "w")
		f.write(diff)
		f.close()
		f = os.popen("patch %s -i %s" %(file_name, file_name + ".diff"))
		return f.read()
	except OSError:
		print "OS error opening {0}".format(file_name + ".diff")
				

