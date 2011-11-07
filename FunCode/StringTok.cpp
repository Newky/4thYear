#include <stdio.h>
#include <cstring>
#include <iostream>
#include <string>
using namespace std;

/* A european string tokeniser
 */
class StringTokeniser {
	private:
		char seperator;
		int current;
	public:
		char * str;
		StringTokeniser(string, char);
		~StringTokeniser();
		char* next();
};

StringTokeniser::StringTokeniser(string sentence, char seperator) {
	this->seperator = seperator;
	this->current = 0;
	str = new char[sentence.size() + 1];
	copy(sentence.begin(), sentence.end(), str);
	str[sentence.size()] = '\0';
};

StringTokeniser::~StringTokeniser() {
	delete[] str;
};

char * StringTokeniser::next() {
	int i=this->current;
	if(this->str[this->current] == '\0')
		return NULL;
	for(;this->str[this->current] != '\0' && this->str[this->current] != this->seperator;this->current++);
	this->str[this->current] = '\0';
	this->current++;
	return this->str+(i);
};

int main() {
	string x = "This is a hello world";
	StringTokeniser*strtok = new StringTokeniser(x, ' ');
	char*current = strtok->next();
	while(current != NULL) {
		printf("%s\n", current);
		current = strtok->next();
	};
	printf("%s", x.c_str());
	return 0;
};

