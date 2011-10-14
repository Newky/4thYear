#!/usr/bin/python
'''
This version doesn't
use the cgi library but sums
all the code.
'''
import os
import sys

data = os.environ

'''
 print out the headers
'''
def headers():
	print "Content-type: text/html\r\n"


def html_wrapper(body):
	print "<html><head><title>CS4032 Dist Systems</title></head><body>"
	print body
	print "</body></html>"

def write_form(path):
	return """
		<form METHOD='POST' action='%s'>
			<input type='text' name='first' value='First Number' />
			<input type='text' name='second' value='Second Number' />
			<input type='submit' />
		</form>
		""" %(path)
headers()
'''
Get environment variables
namely what request method it was (POST/GET)
and the path of the request i.e extension after the host
'''
request_method = os.environ["REQUEST_METHOD"]
path = os.environ["REQUEST_URI"]
'''
If GET request return the form that was generated.
if POST request read from stdin (as this is the body of the message)
parse the input params and generate a result on that input.
'''
if request_method == "GET":
	body = write_form(path)
	html_wrapper(body)
elif request_method == "POST":
	query = sys.stdin.read()
	params = query.split("&")
	first = ""
	second = ""
	for i in params:
		if "first" in i:
			first = i.split("=")[1]
		if "second" in i:
			second = i.split("=")[1]
	body = "%s + %s = %d" %(first, second, (int(first) + int(second)))
	html_wrapper(body)

