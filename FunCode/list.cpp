#include <iostream>
#include <string>
using namespace std;

class Node {
	public:
		Node(string, Node*, Node*);
		~Node();
		Node * next;
		Node * prev;
		string value;
};
/* Node Constructor
 *
 */
Node::Node(string val, Node * prev, Node* next) {
	this->value = val;
	this->next = next;
	this->prev = prev;
};

Node::~Node() {
	if(this->prev != NULL)
		this->prev->next = this->next;
	if(this->next != NULL)
		this->next->prev = this->prev;
};

class Map {

}

int main() {
	Node * head = new Node("Blah", NULL, NULL);
	Node*ptr = head;
	while(ptr->next != NULL) ptr = ptr->next;
	string test[] = {"one", "two", "three", "four"};
	for(int i=0;i<4;i++) {
		ptr->next = new Node(test[i], ptr, NULL);
		ptr = ptr->next;
	}
	ptr = head;
	while(ptr != NULL) {
		cout << ptr->value << endl;
		ptr = ptr->next;
	};
	return 0;
};
