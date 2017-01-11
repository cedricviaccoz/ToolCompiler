#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define INT_MAX_LENGTH 12
#define nRectangle 0

struct Rectangle{
	(int)(*area)(void *);
	int width;
	int height;
};

int Rectangle_area (struct Rectangle* this) {
	return this->width * this->height;
}

void * new(int type){
	void * object;
	switch(type){
		case nRectangle:
			object = malloc(sizeof(struct Rectangle));
			((struct Rectangle*) object)->Rectangle_area;
			break;
		default:
			return NULL;
	}
	return object;
}

int main(void){
	return 0;
}