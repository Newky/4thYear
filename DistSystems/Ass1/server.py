#!/usr/bin/env python
import xmlrpclib
import os
from SimpleXMLRPCServer import SimpleXMLRPCServer
from config import read_in
from files import ls, root_dir



def hello(name):
	config = read_in(name)
	if config == {}:
		return config
	else:
		return config["directories"]


def mount(name, path):
	config = read_in(name)
	root = root_dir(path)
	if config != {}:
		if root in config["directories"]:
			return ls(path)
		else:
			return {
				"status_code" : 0, 
				"error" : "Not Privileged"
				}
	else:
		return {}

def read(name, path):
	config = read_in(name)
	if config != {}:
		if os.path.exists(path):
			with open(path, "rb") as handle:
				return xmlrpclib.Binary(handle.read())

	return xmlrpclib.Binary("");

def function_setup():
	return { 
			"hello" : hello,
			"mount" : mount,
			"read" : read,
		}


def set_up_server(host, port):
	server = SimpleXMLRPCServer((host, port))
	return server


if __name__=="__main__":
	HOST = "localhost"
	PORT = 8080
	server = set_up_server(HOST, PORT);
	func_map = function_setup();
	for k,v in func_map.iteritems():
		server.register_function(v, k)
	server.serve_forever()




