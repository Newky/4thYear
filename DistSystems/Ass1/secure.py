#!/usr/bin/env python
#!/usr/bin/env python
import os

def encrypt_with_key(body, key):
	command = "echo \'%s\' | openssl enc -aes-256-cbc -a -salt -pass pass:%s" %(body, key)
	p = os.popen(command)
	data = p.read().strip()
	p.close()
	return data

def decrypt_with_key(body, key):
	command = "echo \'%s\' | openssl enc -d -aes-256-cbc -a -pass pass:%s" %(body, key)
	p = os.popen(command)
	data = p.read()
	p.close()
	return data

	
	
