#include <stdio.h>
#include <stdlib.h>

using namespace std;

struct ListNode{
    int val;
    ListNode* next;
    ListNode(int i){
        val = i;
        next = NULL;
    }
};
struct List {
    ListNode* head;
    ListNode* tail;
    int length;
    List(){
        head = NULL;
        tail = NULL;
        length = 0;
    }
};

extern "C"{
    void initList(List* l) {
        List* tmp = (List*) malloc(sizeof(List));
        tmp->head = NULL;
        tmp->tail = NULL;
        tmp->length = 0;
        l = tmp;
    };

    void print(List* l){
        ListNode* p = l->head;
        while(p != l->tail){
            printf("%d,", p->val);
            p = p->next;
        }
        if (p){
            printf("%d\n", p->val);
        }
    };

    bool add(List* l, int i) {
        ListNode* n = (ListNode*) malloc(sizeof(ListNode));
        n->val = i;
        n->next = NULL;
        if (l->head == NULL){
            l->head = n;
            l->tail = n;
            l->length = 1;
        } else{
            l->tail->next = n;
            l->tail = l->tail->next;
            l->length += 1;
        }
        return true;
    }
}



