#!/usr/bin/env python
import xmlrpclib
import os
import random

def outputter(name, proxy, path=None):
	if path == None:
		path = os.environ["HOME"]
	return proxy.lookup(name, path)

def output_file(name, file_path, proxy):
	return proxy.read(name, file_path).data

def main():
	name = os.environ["USER"]
	proxy = xmlrpclib.ServerProxy("http://localhost:8080")
	return (name, proxy)

def read_random_file(name, proxy):
	ls = outputter(name, proxy)
	f = random.choice(ls)
	res =outputter(name,proxy, f)
	while(res != ["file"]):
		if(res == []):
			exit(1);
		f = random.choice(ls)
		res =outputter(name,proxy,f)
	return output_file(name, f);

def build_dir(path):
	path = os.path.dirname(path)
	(h, t) = os.path.split(path)
	bits = []
	bits.append(t)
	while h!= '' and t != '':
		(h, t) = os.path.split(h)
		bits.append(t)
	bits.reverse()
	bits[0] = "cached"
	acc_path = ""
	for crumb in bits:
		acc_path = os.path.join(acc_path, crumb)
		if os.path.exists(os.path.join(os.getcwd(), acc_path)):
			continue
		else:
			try:
				os.mkdir(os.path.join(os.getcwd(), acc_path))
			except OSError:
				return None
	return "cached";
	
'''
Build path
step by step
in a cache.

Then some form of trigger will send back changes to the user
Save two seperate cached file
'''

def read_file(name, path, proxy):
	if build_dir(path) != None:
		data = output_file(name, path, proxy)
		cached_path = os.path.join(os.getcwd(), os.path.join("cached", path[1:]) )
		print "Path saved at "+cached_path
		f = open(cached_path, "w")
		fc = open(cached_path + ".cache", "w")
		f.write(data)
		fc.write(data)
		f.close()
		fc.close()

	else:
		return None


if __name__ == "__main__":
	(name, proxy) =main()
	#read_random_file(name, proxy)
	full_path = os.path.join(os.environ["HOME"], "test.txt");
	#exit(proxy.insert(name,full_path));
	#exit(proxy.remove(name,full_path));
	read_file(name, full_path, proxy)
	


