URL="http://richydelaney.com:9888"
FILE="test.txt"
USERNAME="ec2-user"

setup:
	make clean
	make venus 
	echo "Now start watcher"
test:
	echo `date` >> cached/home/${USERNAME}/test.txt
venusplusone:
	cd cached/ && python ../venus.py ${URL} ${FILE} ${USERNAME}
venus:
	python venus.py ${URL} ${FILE} ${USERNAME}
vice:
	python vice.py
clean:
	rm -r cached/
watcher:
	python watcher ${URL}
