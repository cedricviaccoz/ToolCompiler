#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define INT_MAX_LENGTH 12
#define nFact 0

struct Fact{
	(int)(*computeFactorial)(void *, int);
};

int Fact_computeFactorial (struct Fact* this, int num) {
	if(num < 1)
		num_aux = 1;
	else
		num_aux = num * this->computeFactorial(num - 1);

	return num_aux;
}

void * new(int type){
	void * object;
	switch(type){
		case nFact:
			object = malloc(sizeof(struct Fact));
			((struct Fact*) object)->Fact_computeFactorial;
			break;
		default:
			return NULL;
	}
	return object;
}

int main(void){
printf(strcat(strcpy(malloc(strlen("value") + sizeof(new(Fact)->computeFactorial(10)) + 1), "value"), itoa(new(Fact)->computeFactorial(10))));
	return 0;
}


// helper functions for the concatenation of a string of characters and an int:

void helper_reverse_plus(char str[], int len) {
	int start;
	int end;
	char temp;
	for(start = 0, end = len-1; start < end; start++, end--) {
		temp = *(str+start);
		*(str+start) = *(str+end);
		*(str+end) = temp;
	}
}

char* itoa(int num) {
	int i = 0;
	int isNegative = 0;
	char* str = malloc(sizeof(num));

	if (num == 0) {
		str[i] = '0';
		str[i + 1] = '\0';
		return str;
	}

	if (num < 0) {
		isNegative = 1;
		num = -num;
	}

	while (num != 0) {
		int rem = num % 10;
		str[i++] = (rem > 9) ? (rem - 10) + 'A' : rem + '0';
		num = num/10;
	}

	if (isNegative) {
		str[i++] = '-';
	}

	str[i] = '\0';
	helper_reverse_plus(str, i);
	return str;
}
