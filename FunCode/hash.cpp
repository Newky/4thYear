#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

/* WordPair is a single word
 * to many word pairs.
 * Used to store the prefix and its
 * many suffix's for the markov chain.
 */
class WordPair {
	public:
		WordPair(string, string);
		~WordPair();
		string random_item();
		string word;
		vector<string>* matches;
};
/* Constructor for WordPair
 * Takes a string for word
 * and the first string for the vector of strings
 */
WordPair::WordPair(string lh, string rh) {
	this->word = lh;
	this->matches = new vector<string>();
	this->matches->push_back(rh);
};

/* Nothing here at the minute.
 */
WordPair::~WordPair() {
};

/* Other than storing more suffixes
 * all we need is to get a random item
 * from the vector.
 * NOTE: if matches for some reason goes beyond
 * INT_MAX, the bounds will default to 0 --> INT_MAX
 */
string WordPair::random_item() {
	int size = (int) this->matches->size();
	int n = rand() % size;
	string x = this->matches->at(n);
	return x;
};

class Dict {
	private:
		int hash(string key);
		int size;
		vector<WordPair>** values;
	public:
		Dict(int n);
		~Dict();
		vector<string>* get(string key);
		void put(string key, string value);
};

/* Constructor for Dict
 * Initialise with an array size of n
 * Each element of the array is a
 * vector of WordPairs (actually a pointer to
 * a vector of WordPairs.
 */
Dict::Dict(int n) {
	this->size = n;
	this->values =  new vector<WordPair>*[this->size];
	int i=0;
	for(;i<this->size;i++)
		this->values[i] = new vector<WordPair>();
};
/* Deconstruct each of the vectors.
 * Deconstruct the array of vectors.
 */
Dict::~Dict() {
	int i=0;
	for(;i<this->size;i++)
		delete this->values[i];
	delete this->values;
};
/* Just a hash function
 * Questionable how wonderful it is, 
 * But as far as I know its similar to
 * the hash code for hashing strings in
 * Java.
 * 31 is used because its prime. Its not widely
 * known why primes give the best results 
 * for hash functions.
 */
int Dict::hash(string key) {
	int i = 0;
	int res = 0;
	const char * str = key.c_str();
	for(;str[i]!='\0';i++)
		res = res + (str[i] * 31);

	return res % this->size;
	
};

/* To put an entry into the dictionary, we must 
 * first hash the string, get out the array of
 * vectors.
 * Each vector from this vector (Sorry a bit cryptic)
 * will hold a WordPair.
 */
void Dict::put(string key, string value) {
	int x = this->hash(key);
	vector<WordPair>* vwp = this->values[x];
	if( vwp->size() == 0) {
		vwp->push_back(WordPair(key, value));
	}else{
		vector<WordPair>::iterator it = vwp->begin();
		int found = 0;
		for(;it != vwp->end(); ++it) {
			if(it->word.c_str() == key) {
				it->matches->push_back(value);
				found = 1;
				break;
			}
		}
		if(!found) {
			vwp->push_back(WordPair(key, value));
		}
	}
};

vector<string>* Dict::get(string key) {
	int x = this->hash(key);
	vector<WordPair>* vwp = this->values[x];
	if( vwp->size() == 0) {
		return NULL;
	}else {
		vector<WordPair>::iterator it = vwp->begin();
		int found = 0;
		for(;it != vwp->end(); ++it) {
			if(it->word.c_str() == key) {
				return it->matches;
			}
		}
		return NULL;
	}
}

int main() {
	WordPair* x = new WordPair((string)"hello", (string)"world");
	printf("%s", x->random_item().c_str());
	Dict* dict = new Dict(10);
	dict->put((string) "hello", (string)"world");
	dict->put((string) "hello", (string)"u");
	vector<string>* res = dict->get((string) "hello");
	if(res != NULL) {
		vector<string>::iterator it = res->begin();
		for(;it != res->end(); ++it) {
			printf("%s\n", it->c_str());
		}
	}
	delete dict;
	delete x;
	return 0;
}


