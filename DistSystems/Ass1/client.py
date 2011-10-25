#!/usr/bin/env python
import xmlrpclib
import os

name = "richy"
proxy = xmlrpclib.ServerProxy("http://localhost:8080")
directories = proxy.hello(name);
files= proxy.mount(name, directories[0])
for i in range(0, len(files)):
	print "%d) %s" %(i, files[i])
print "Select file to open:"
i = raw_input("File no:")
while i < len(files):
	i = raw_input("No such file number, File no:")
i = int(i)
path = directories[0] + files[i]
if os.path.exists(path):
	open(path, "w").close()

with open(files[i], "wb") as handle:
	handle.write(proxy.read(name, "%s" %(path) ).data)


