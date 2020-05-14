#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

extern "C"{

    struct node {
        struct node *next;
        int val;
    };

    struct Set {
        struct node *head;
        struct node *tail;
        struct node *iter;
        struct node *iter_next;
        int num;
        int num_adts;
    };

    struct Set * set_init(void){
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

    static struct node * node_init(void){
        struct node *new_node=NULL;
        new_node = (struct node *) malloc(sizeof(struct node));
        new_node->next = NULL;
        return new_node;
    }

    static void node_free(struct node * n){
        free(n);
        n=NULL;
    }

    struct node * first(struct Set *s){
        s->iter = s->head;
        if(s->head){
            s->iter_next = s->head->next;
        }
        return s->head;
    }

    struct node * end(struct Set *s){
        return s->iter?s->iter:NULL; 
    }

    struct node * next(struct Set *s){
        s->iter = s->iter_next;
        if(s->iter_next)
            s->iter_next = s->iter_next->next;
        return s->iter;
    }

    int exist(struct Set *s, int i){
        struct node * n;
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

        struct node * new_node = node_init();
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
        struct node *n, *del, *last=NULL;
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
}