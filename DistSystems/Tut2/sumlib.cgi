#!/usr/bin/python
'''
This is the version of the summing
cgi program using the cgi library to
handle the form variables coming in.
the difference in the imports is
the cgi and the cgitb which
produces useful debugging errors to
the stdout
'''
import os
import sys
import cgi
import cgitb; cgitb.enable()
'''
 print out the headers
'''
def headers():
	print "Content-type: text/html\r\n"

'''
Wraps the html body
in the normal markup
'''
def html_wrapper(body):
	print "<html><head><title>CS4032 Dist Systems</title></head><body>"
	print body
	print "</body></html>"
'''
Return a html form with two input textboxes
and set the action as the path
'''
def write_form(path):
	return """
		<form METHOD='POST' action='%s'>
			<input type='text' name='first' value='First Number' />
			<input type='text' name='second' value='Second Number' />
			<input type='submit' />
		</form>
		""" %(path)

'''
Get the request and path environment variables
'''
request_handler = os.environ["REQUEST_METHOD"]
path = os.environ["REQUEST_URI"]
'''
Print the headers
'''
headers()
'''
If post handle the incoming form,
otherwise print the form
'''
if request_handler == "POST":
	form = cgi.FieldStorage()
	if form.has_key("first") and form.has_key("second"):
		first = form["first"].value
		second= form["second"].value
		body = "%s + %s = %d" %(first, second, (int(first) + int(second)))
		html_wrapper(body)
	else:
		body = "Masssive Fail with the post params!"
		html_wrapper(body)
else:
	form = write_form(path)
	html_wrapper(form)
