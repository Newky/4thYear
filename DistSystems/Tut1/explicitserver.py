#!/usr/bin/env python

# Do the necessary imports
import socket
import thread
#Port and host to bind
PORT = 80001
HOST = "127.0.0.1"
#List of explicit words to match
explicits = [
	"Hello"
	];

'''
this function deals with the incoming string
Checks the information against the listed explicits
Search and replace the data and return the new string
'''
def deal_with_data(data):
	for i in explicits:
		data =  data.replace(i, xs(i))
	return data
'''
Censor the word
'''
def xs(word):
	data = ""
	for i in word:
		data+= "x"
	return data

'''
This function takes a connection and deals with it
and returns the censored information.
'''
def respond( conn, address):
	while 1:
		data = conn.recv(1024)
		conn.send(deal_with_data(data))
		if not data:
			conn.close()
			break
		
#Setup a new socket
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM);
#When the server is exited, give up address.
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
#Bind the socket to the desired host and port
s.bind((HOST, PORT))
#listen for connections
s.listen(5)
while 1:
    #Accept forks a new connection and address for every incoming connection
    conn, address = s.accept()
    #Start a new thread to deal with the new connection
    thread.start_new_thread(respond, (conn, address))

