#!/usr/bin/env python
import os
import pyinotify
import sys
import xmlrpclib

SERVER_URL = "http://localhost:8080/"

class MyEventHandler(pyinotify.ProcessEvent):
    def __init__(self, name, proxy, file_object=sys.stdout):
        """
        This is your constructor it is automatically called from ProcessEvent.__init__(),
        And extra arguments passed to __init__() would be delegated automatically to 
        my_init().
        """
	print "Init"
	self.name = name
	self.proxy = proxy
        self._file_object = file_object

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
					ret= self.proxy.patch(self.name, uncached, lines)
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

def main():
	name = os.environ["USER"]
	proxy = xmlrpclib.ServerProxy(SERVER_URL)
	return (name, proxy)

if __name__ == "__main__":
	if len(sys.argv) > 1:
		SERVER_URL = sys.argv[1]
	(name, proxy) = main();
	name = "ec2-user"
	wm = pyinotify.WatchManager()
	event_handler = MyEventHandler(name, proxy)
	notifier = pyinotify.Notifier(wm, event_handler)
	wm.add_watch("cached/", pyinotify.IN_CLOSE_WRITE, rec=True)
	notifier.loop()
