#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

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
            l->length++;
        }
        return true;
    };

    int get(List* l, int idx){
        assert(idx < l->length);
        int i = 0;
        ListNode* p = l->head;
        while (p && i < idx){
            p = p->next;
            i++;
        }
        return p->val;
    };

    bool removeList(List* l, int idx){
        if (idx >= l->length){
            return false;
        }
        ListNode* tmp;
        if (idx == 0){
            tmp = l->head;
            l->head = l->head->next;
        }
        else{
            int i = 0;
            ListNode* p = l->head;
            while (p && i < idx - 1){
                p = p->next;
                i++;
            }
            if (p->next->next){
                tmp = p->next;
                p->next = p->next->next;
            } else{
                tmp = p;
                p->next = NULL;
            }
        }
        free(tmp);
        l->length--;
        return true;
    };

    bool set(List* l, int idx, int val){
        if (idx >= l->length){
            return false;
        }
        int i = 0;
        ListNode* p = l->head;
        while (p && i < idx){
            p = p->next;
            i++;
        }
        p->val = val;
        return true;
    };

    bool insert(List* l, int idx, int val){
        if (idx >= l->length){
            return false;
        }
        if (idx == 0){
            ListNode* n = (ListNode*) malloc(sizeof(ListNode));
            n->val = val;
            n->next = l->head;
            l->head = n;
            l->length++;
            return true;
        }
        else{
            int i = 0;
            ListNode* p = l->head;
            while (p && i < idx - 1){
                p = p->next;
                i++;
            }
            ListNode* q = p->next;
            ListNode* n = (ListNode*) malloc(sizeof(ListNode));
            n->val = val; n->next = NULL;
            p->next = n;
            n->next = q;
            l->length++;
            return true;
        }
    };

    int sizeofList(List* l){
        return l->length;
    }
}



