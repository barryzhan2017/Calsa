#include <stdio.h>

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
    };

    
};

extern "C"{
    void initList(List* l) {
        printf("init a list\n");
        List tmp = List();
        l = &tmp;
    };

    void print(List* l){
        ListNode* p = l->head;
        while(p != l->tail){
            printf("%d,", p->val);
        }
        if (p){
            printf("%d\n", p->val);
        }
    };

    bool add(List* l, int i) {
        ListNode n = ListNode(i);
        if (l->head == NULL){
            l->head = &n;
            l->tail = &n;
        } else{
            l->tail->next = &n;
        }
        l->length++;
        printf("adding sth\n");
        return true;
    }
}



