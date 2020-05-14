#include <stdlib.h>
#include <stdio.h>

typedef struct ListNode {
    int val;
    struct ListNode *next;
} list_node;

typedef struct List {
    int32_t length;
    list_node *head;
    list_node *tail;
} list;

// initialize the list with a dummy node
// list looks like d(head) -> 1 -> 2 -> 3(tail)
void list_init(list *l) {
    printf("Init in\n");
    l->head = (list_node *) malloc(sizeof(list_node));
    l->tail = l->head;
    l->length = 0;
}

list* list_create() {
    printf("Create in\n");
    list *l = (list *) malloc(sizeof(list));
    l->head = (list_node *) malloc(sizeof(list_node));
    l->tail = l->head;
    l->length = 0;
    return l;
}

void list_append(list *l, int val) {
    printf("Append in\n");
    list_node *new_node = malloc(sizeof(list_node));
    new_node->val = val;
    new_node->next = NULL;
    l->tail->next = new_node;
    l->tail = new_node;
    l->length += 1;
}

int list_sum(list *l) {
    printf("Sum in\n");
    int s = 0;
    list_node *curr = l->head->next;
    while (curr) {
        s += curr->val;
        curr = curr->next;
    }
    return s;
}



int add(list *l) {
	printf("In!\n");
    list_append(l, 12);
    list_append(l, 2);
    list_append(l, 3);
    printf("%d\n", list_sum(l));
    return 1;
}