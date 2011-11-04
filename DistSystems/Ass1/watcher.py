#!/usr/bin/env python
import sys
import pyinotify
import os


class MyEventHandler(pyinotify.ProcessEvent):
    def my_init(self, file_object=sys.stdout):
        """
        This is your constructor it is automatically called from ProcessEvent.__init__(),
        And extra arguments passed to __init__() would be delegated automatically to 
        my_init().
        """
        self._file_object = file_object

    def process_IN_CLOSE_WRITE(self, event):
	"""
	what happens when you write something
	"""
	print "Fired"
	if os.path.exists(event.pathname):
		if os.path.exists(event.pathname + ".cache"):
			os.popen("diff %s %s > %s" %(event.pathname, event.pathname + ".cache", event.pathname + ".diff"))

    def process_default(self, event):
        """
        Eventually, this method is called for all others types of events.
        This method can be useful when an action fits all events.
        """
	
        self._file_object.write('default processing\n')

# A way to instantiate this class could be:
if __name__ == "__main__":
	wm = pyinotify.WatchManager()
	event_handler = MyEventHandler()
	notifier = pyinotify.Notifier(wm, event_handler)
	wm.add_watch("cached/", pyinotify.IN_CLOSE_WRITE, rec=True)
	notifier.loop()
