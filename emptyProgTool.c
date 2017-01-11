#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define INT_MAX_LENGTH 12
#define nA 0
#define nB 1

struct A{
	(int)(*foo)(void *, int);
	int b;
	int * c;
};

int A_foo (struct A* this, int d) {
	c = calloc(4, sizeof(int));
	c[0] = 2;
	return 0;
}
struct B{
	(int)(*foo)(void *, int);
	int b;
	int * c;
	(int)(*bar)(void *, int *, char *);
	char * t;
};

int B_bar (struct B* this, int * d, char * r) {
	return 1;
}

void * new(int type){
	void * object;
	switch(type){
		case nA:
			object = malloc(sizeof(struct A));
			((struct A*) object)->A_foo;
			break;

		case nB:
			object = malloc(sizeof(struct B));
			((struct B*) object)->A_foo;
			((struct B*) object)->B_bar;
			break;
		default:
			return NULL;
	}
	return object;
}

int main(void){
	return 0;
}