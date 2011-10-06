#!/usr/bin/env python
import socket

HOST = "127.0.0.1"
PORT = 80001
'''
Start a new socket,
bind it to the servers address and port
ask the user for a message
'''
if __name__ == "__main__":
	conn = socket.socket()
	conn.connect((HOST, PORT))
	user_msg = raw_input(">>>")
	conn.send(user_msg)
	data = conn.recv(1024)
	print data

