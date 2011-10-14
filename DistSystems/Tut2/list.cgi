#!/usr/bin/python
#Import the os module
import os
'''
Prints out the headers of the message and the html body
followed by a table which contains all of the keys of 
the environment variables which are held in
os.environ
'''
print "Content-type: text/html\r\n"
print "<html><head><title>CGI environ</title></head><body><table><tbody>"
print "<tr><td>%s</td><td>%s</td></tr>" %("Key", "Value")
keys= os.environ.keys()
data = os.environ
keys.sort()
for i in keys:
	print "<tr><td>%s</td><td>%s</td></tr>" %(i, data[i])
print "</tbody></table></body></html>"

