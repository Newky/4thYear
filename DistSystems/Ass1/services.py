#!/usr/bin/env python
import base64
import os
import json
import socket
import secure
import sys
import threading

dslookups = {}
aslookups = {}
TIMEOUT = 5.0

def hidden_file_path(file_path):
	if file_path.rfind("/") == -1:
		return "." + file_path
	else:
		return file_path[0:file_path.rfind("/")+1] + "." + file_path[file_path.rfind("/")+1:] 


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

def unlock_file(file_to_lookup, name, password, HOST="localhost", PORT=9998, session=None):
	global aslookups
	# AS LOOKUP
	# FOR LS
	ticket, session, server_id = check_as_cache(name, password, "ls", HOST, PORT) 
	ls_results = lookup_ls(file_to_lookup, "unlock", server_id, ticket, session, name)
	if "status_code" in ls_results:
		if ls_results["status_code"] == 0:
			raise IOError(ls_results["message"])
	else:
		return None

def check_as_cache(name,password,service, HOST, PORT):
	global aslookups
	if service in aslookups:
		print "Using Cached AS lookup for {0}".format(service)
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

def get_ticket_for_file(file_to_lookup, name, password, HOST="localhost", PORT=9998, session=None):
	global aslookups, dslookups
	# AS LOOKUP
	# FOR LS
	ticket, session, server_id= check_as_cache(name,password,"ls", HOST, PORT) 
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
		#print "Using Cached DS lookup for "+file_to_lookup
		message = dslookups[os.path.dirname(file_to_lookup)]
	else:
		# Lookup the directory service for the file we are looking for
		message = lookup_ds(file_to_lookup, server_id, ticket, session)
		# json decode the message coming back
		message= json.loads(message)
		dslookups[os.path.dirname(file_to_lookup)] = message
	print "message from ds lookup:{0}".format(message)
	#Ticket for FS from DS
	ticket = message["ticket"]
	server_id = json.loads(message["message"])

	return (ticket, session, server_id)

def lookup_fs(data, local_file, server_id, password):
	received = None
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	try:
		#print "About to connect socket"
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
		#print "socket connected, going into loop"
		while 1:
			packet = sock.recv(1024)
			if not packet:  break
			received += packet
		#print "Here and now"
		received=secure.decrypt_with_key(received, password)
		received=json.loads(received)
	finally:
		sock.close()
		#print "Data {0}".format(data)
		#print "Received {0}".format(received)
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
		else:
			return None

def check_servers_up(services):
	print "Checking Servers"
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
		services[serv] = filter((lambda x: x!= None), service)
	return services

class TaskThread(threading.Thread):
    """Thread that executes a task every N seconds"""
    
    def __init__(self):
        threading.Thread.__init__(self)
	threading.Thread.daemon = True
        self._finished = threading.Event()
        self._interval = 60.0
    
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
					
	
def patch_file(file_name, diff):
	try:
		f = open(file_name + ".diff", "w")
		f.write(diff)
		f.close()
		f = os.popen("patch %s -i %s" %(file_name, file_name + ".diff"))
		return f.read()
	except OSError:
		print "OS error opening {0}".format(file_name + ".diff")
				

