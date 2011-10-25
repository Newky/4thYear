#!/usr/bin/env python
import os

def ls(path):
	try:
		files = os.listdir(path)
	except OSError:
		return []
	return files


