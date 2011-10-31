#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

class WordPair {
	public:
		WordPair(string, string);
		~WordPair();
		string random_item();
		string word;
		vector<string> matches;
};

WordPair::WordPair(string lh, string rh) {
	this->word = lh;
	this->matches = vector<string>();
	this->matches.push_back(rh);
};

string WordPair::random_item() {
	int size = (int) this->matches.size();
	int n = rand() % size;
	string x = this->matches.at(n);
	return x;
};

//class Dict {
	//private:
		//int hash(string key);
		//int size;
		//vector<WordPair> values[];
	//public:
		//Dict(int n);
		//~Dict();
		//string get(string key);
		//void put(string key, string value);
//};

///* Constructor for Dict
 //* Initialise with an array size of n
 //*/
//Dict::Dict(int n) {
	//this->size = n;
	////this->values = new vector<WordPair>[this->size];
//};

//Dict::~Dict() {
//};

/* To put an entry into the dictionary, we must 
 * first hash the string, get out the array of
 * vectors.
 * Each vector from this vector (Sorry a bit cryptic)
 * will hold a WordPair.
 */
//void Dict::put(string key, string value) {
	//int x = hash(key);
	//vector temp = this->values.begin();
	//while(temp.word == key){

	//}
//};

int main() {
	WordPair* x = new WordPair((string)"hello", (string)"world");
	printf("%s", x->random_item().c_str());
	return 0;
}


