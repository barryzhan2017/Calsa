#include <stdlib.h>
#include <stdio.h>

struct List {
    char* val;
    struct List* next;
};

int add(struct List list) {
	printf("In!\n");
    return 1;
}


