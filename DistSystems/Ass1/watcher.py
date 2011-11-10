#!/usr/bin/env python
import os
import pyinotify
import sys
import xmlrpclib
from hashlib import sha1

SERVER_URL = "http://localhost:8080/"

class MyEventHandler(pyinotify.ProcessEvent):
    def __init__(self, name, proxy, uniq_id,file_object=sys.stdout):
        """
        This is your constructor it is automatically called from ProcessEvent.__init__(),
        And extra arguments passed to __init__() would be delegated automatically to 
        my_init().
        """
	print "Init"
	self.name = name
	self.proxy = proxy
        self._file_object = file_object
	self.open_files = list_of_open()
	self.processing= []
	self.uniq_id = uniq_id

    def process_IN_OPEN(self, event):
	if os.path.exists(event.pathname):
		uncached = event.pathname[event.pathname.index("cached")+6:]
		cached= "cached" + uncached
		if os.path.isdir(event.pathname):
			pass	
		elif cached in self.open_files:
			mtime = os.path.getmtime(cached + ".cache")
			if self.proxy.valid(name, uncached, self.uniq_id, mtime):
				print "A file was opened because it was valid. (%s)" %(event.pathname)
				pass
			elif cached in self.processing:
				pass
			else:
				self.processing.append(cached)
				try:
					f = open(cached+".diff", "w");
					data = self.proxy.read(self.name, cached+".diff").data
					f.write( data )
					f.close()
					os.popen('patch %s -i %s' %(cached, cached + ".diff"))
				except OSError:
					print "error"
					self.processing.remove(cached)
				except IOError:
					print "IOerror"
					self.processing.remove(cached)
				print "File not valid"

    def process_IN_CLOSE_WRITE(self, event):
	"""
	what happens when you write something
	"""
	print "Fired"
	if os.path.exists(event.pathname):
		if os.path.exists(event.pathname + ".cache"):
			os.popen("diff %s %s > %s" %( event.pathname + ".cache", event.pathname,event.pathname + ".diff"))
			try:	
				lines = open(event.pathname + ".diff", "r").read();
				try:
					uncached = event.pathname[event.pathname.index("cached")+6:]
					print "%s" %(uncached)
					ret= self.proxy.patch(self.name, uncached, lines, self.uniq_id)
					f = os.popen('patch %s -i %s' %(event.pathname + ".cache", event.pathname + ".diff"))
				except ValueError:
					print "Something went wrong with the Value."
			except IOError as (errno, strerror):
				print strerror
				print "Something went wrong with the IO." + event.pathname

    def process_default(self, event):
        """
        Eventually, this method is called for all others types of events.
        This method can be useful when an action fits all events.
        """
        self._file_object.write('default processing\n')

def list_of_open(path = "cached/"):
	files = []
	for dirname, dirnames, filenames in os.walk(path):
		for file_name in filenames:
			if not ".cache" in file_name:
				if not ".diff" in file_name:
					files.append(os.path.join(dirname, file_name))
	return files

				

def main():
	proxy = xmlrpclib.ServerProxy(SERVER_URL)
	return proxy 


if __name__ == "__main__":
	name = os.environ["USER"]
	if len(sys.argv) > 1:
		SERVER_URL = sys.argv[1]
		if len(sys.argv) > 2:
			name = sys.argv[2]
	proxy = main()
	uniq_id = sha1(os.environ["HOME"]).hexdigest()
	wm = pyinotify.WatchManager()
	event_handler = MyEventHandler(name, proxy, uniq_id)
	notifier = pyinotify.Notifier(wm, event_handler)
	mask = pyinotify.IN_CLOSE_WRITE | pyinotify.IN_OPEN
	wm.add_watch("cached/", mask, rec=True)
	notifier.loop()
