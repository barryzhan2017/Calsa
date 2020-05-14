#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>

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

    bool find(List* l, int val) {
        ListNode* p = l->head;
        while(p != l->tail){
            if (p->val == val) {
                return true;
            } 
            p = p->next;
        }
        return false;
    }

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

    int sumList(List *l) {
        int res = 0;
        ListNode* p = l->head;
        while (p) {
            res += p->val;
            p = p->next;
        }
        return res;
    }

    int maxList(List *l) {
        int res = INT_MIN;
        ListNode* p = l->head;
        while (p) {
            res = res > p->val ? res : p->val;
            p = p->next;
        }
        return res;
    }

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

// Definition for hash table
#define TABLE_SIZE 7919
static int hashFunc(int key) {
    return key >= 0 ? key % TABLE_SIZE: (-key) % TABLE_SIZE;
}

struct HashtableNode {
    int key, value;
    struct HashtableNode *next;
    HashtableNode() {
        key = 0;
        value = 0;
        next = NULL;
    }
};
struct Hashtable {
    int size;
    struct HashtableNode **data;
    Hashtable() {
        data = NULL;
        size = 0;
    }
};

extern "C" {
    void initHashtable(struct Hashtable *ht) {
        ht->data = (struct HashtableNode**) malloc(TABLE_SIZE * sizeof(struct HashtableNode*));
        int i;
        for (i = 0; i < TABLE_SIZE; ++i) {
            ht->data[i] = NULL;
        }
    }
    bool hasKey(struct Hashtable* ht, int key) {
        int pos = hashFunc(key);
        struct HashtableNode *p = ht->data[pos];
        while (p) {
            if (p->key == key) {
                return true;
            }
        }
        return false;
    }
    int getV(struct Hashtable* ht, int key) {
        int pos = hashFunc(key);
        struct HashtableNode *p = ht->data[pos];
        while (p) {
            if (p->key == key) {
                return p->value;
            }
            p = p->next;
        }
        return INT_MIN;
    }
    void setKV(struct Hashtable* ht, int key, int val) {
        int pos = hashFunc(key);
        struct HashtableNode *p = ht->data[pos];
        if (!p) { // if row for that table is empty
            ht->data[pos] = (struct HashtableNode*) malloc(sizeof(struct HashtableNode));
            ht->data[pos]->value = val;
            ht->data[pos]->key = key;
            ht->data[pos]->next = NULL;
            ht->size++;
        }
        else {
            while (p) {  // check if key already exist
                if (p->key == key) {
                    p->value = val;
                    return;
                }
                p = p->next;
            }
            // if key does not exist, add a new node
            struct HashtableNode* node = (struct HashtableNode*) malloc(sizeof(struct HashtableNode));
            node->key = key;
            node->value = val;
            node->next = ht->data[pos];
            ht->data[pos]->next = node;
            ht->size++;
        }
        
    }
    void printHashtable(struct Hashtable* ht) {
        int i;
        printf("{");
        for (i = 0; i < TABLE_SIZE; ++i) {
            struct HashtableNode *p = ht->data[i];
            while (p != NULL) {
                printf("%d->%d, ", p->key, p->value);
                p = p->next;
            }
        }
        printf("}\n");
    }
}