#include "Norem.h"

Term_t* CPS_2() {
    Term_t* a;
    Term_t* b;
    Term_t* k;
    
    a = pop();
    b = pop();
    k = pop();

    return k;
}

Term_t* iADD() {
    Term_t* a = pop();
    Term_t* b = pop();
    Term_t* k = pop();

    if(a == NULL || b == NULL || k == NULL) {
        error("expect two data and a continuation!\n");
    }

    if(a->tag != INT || b->tag != INT) {
        error("ADD_i takes two integer!\n");
    }

    return _App(k,_Int(a->as_int + b->as_int));
}

Term_t* iPRINT() {
    Term_t* a = pop();
    Term_t* k = pop();
    
    if(a == NULL || k == NULL) {
        error("expect a data and a continuation!\n");
    }

    if(a->tag != INT) {
        error("iPRINT takes a integer!\n");
    }

    printf("iPRINT: %d\n",a->as_int);
    return k;
}

Term_t* EXIT() {
    exit(0);
}

Term_t* PAIR() {
    Term_t* h = pop();
    Term_t* t = pop();
    Term_t* k = pop();

    if(h == NULL || t == NULL || k == NULL) {
        error("expect two data and a continuation!\n");
    }

    return _App(k,_Cons(h,t));
}

Term_t* HEAD() {
    Term_t* p = pop();
    Term_t* k = pop();

    if(p == NULL || k == NULL) {
        error("expect a pair and a continuation!\n");
    }

    if(p->tag != CONS) {
        error("HEAD takes a integer!\n");
    }

    return _App(k,p->t1);
}


Term_t* TAIL() {
    Term_t* p = pop();
    Term_t* k = pop();

    if(p == NULL || k == NULL) {
        error("expect a pair and a continuation!\n");
    }

    if(p->tag != CONS) {
        error("HEAD takes a integer!\n");
    }

    return _App(k,p->t2);
}