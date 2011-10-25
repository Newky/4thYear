#!/usr/bin/env python
import xmlrpclib
import os

def cd(proxy, name, current_dir, args):
	current_dir = current_dir + args
	if current_dir[-1:] != "/":
		current_dir += "/"
	return [current_dir, "Changed to "+current_dir]

def ls(proxy, name, current_dir, args):
	[directories, files] = proxy.mount(name, current_dir)
	return [current_dir, ("\n".join(directories))+"\n\n"+("\n".join(files))]

def less(proxy, name, current_dir, args):
	path = current_dir + args
	return [current_dir, proxy.read(name, "%s" %(path)).data]

def helo(proxy, name):
	directories = proxy.hello(name);
	return directories

def main():
	name = os.environ["USER"]
	proxy = xmlrpclib.ServerProxy("http://localhost:8080")
	available = helo(proxy, name)
	return (name, proxy, available[0])

commands = {
	"ls" : ls,
	"less" : less,
	"cd" : cd 
   }


if __name__ == "__main__":
	(name, proxy, current_dir) = main()
	cmd = raw_input(">>>")
	while (cmd != "exit"):
		cmd = cmd.strip()
		parts = cmd.split(" ", 1)
		if len(parts) == 1:
			[core, args] = [parts[0], ""]
		else:
			[core, args] = parts
		try:
			[current_dir, msg] = commands[core](proxy, name, current_dir, args)
			print msg
		except KeyError:
			pass	
		cmd = raw_input(">>>")


