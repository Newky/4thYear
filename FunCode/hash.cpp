#include <stdio.h>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

class Pair {
	public:
		string word;
		vector<string> matches;
};

class Dict {
	private:
		int hash(string key);
		int size;
		vector< vector<Pair> > values;
	public:
		Dict(int n);
		~Dict();
		string get(string key);
		void put(string key, string value);
};

/* Constructor for Dict
 * Initialise with an array size of n
 */
Dict::Dict(int n) {
	this->size = n;
	this->values.resize(this->size);
};

Dict::~Dict() {
};

//void Dict::put(string key, string value) {
	//int x = hash(key);
	//vector temp = this->values.begin();
	//while(temp.word == key){

	//}
//};

int main() {

	return 0;
}


