#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

extern "C"{

    struct Node {
        struct Node *next;
        int val;
    };

    struct Set {
        struct Node *head;
        struct Node *tail;
        struct Node *iter;
        struct Node *iter_next;
        int num;
        int num_adts;
    };

    struct Set * set_init(){
        struct Set *set;
        set = (struct Set *) malloc(sizeof(struct Set));

        set->head = NULL;
        set->tail = NULL;
        set->iter = NULL;
        set->iter_next = NULL;

        set->num = 0;
        set->num_adts = 0;

        return set;
    }

    static struct Node * node_init(void){
        struct Node *new_node=NULL;
        new_node = (struct Node *) malloc(sizeof(struct Node));
        new_node->next = NULL;
        return new_node;
    }

    static void node_free(struct Node * n){
        free(n);
        n=NULL;
    }

    struct Node * first(struct Set *s){
        s->iter = s->head;
        if(s->head){
            s->iter_next = s->head->next;
        }
        return s->head;
    }

    struct Node * end(struct Set *s){
        return s->iter?s->iter:NULL; 
    }

    struct Node * next(struct Set *s){
        s->iter = s->iter_next;
        if(s->iter_next)
            s->iter_next = s->iter_next->next;
        return s->iter;
    }

    int exist(struct Set *s, int i){
        struct Node * n;
        for(n = first(s); end(s); n = next(s)){
            if(n->val == i){
                return 1;
            }
        }
        return 0;
    }

    bool add(struct Set *s, int i) {

        if(exist(s, i)){ 
            printf("element already exists!");
            return false; 
        }

        struct Node * new_node = node_init();
        s->num += 1;

        //add to list
        if(s->head == NULL){
            s->tail = new_node;
            s->head = new_node;
        } else {
            s->tail->next = new_node;
            s->tail = new_node;
        }
        return true;
    }

    bool remove(struct Set * s, int i){
        struct Node *n, *del, *last=NULL;
        for(n = first(s); end(s); n = next(s)){            
            if(n->val == i){
                del=n;

                if(del==s->head && del==s->tail){
                    s->head = del->next;
                    s->tail = last;

                    if(s->tail){
                        s->tail->next = NULL;
                    }

                } else if(del==s->head){
                    s->head = del->next;
                } 
                
                else if(del==s->tail){
                    s->tail = last;

                    if(s->tail){
                        s->tail->next = NULL;
                    }

                }
                else {
                    last->next = del->next;
                }
                
                node_free(del);
                s->num--;
                return true;
            }
            last = n;
        }
        return false;
    }

    int size(struct Set * s){
        return s->num;
    }
}