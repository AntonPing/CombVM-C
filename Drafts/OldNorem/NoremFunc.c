#include "Norem.h"

/*
Term_t* DEFINE() {



}
*/


Term_t* IFTE() {
    Term_t* p = purify(pop());
    Term_t* t = pop();
    Term_t* f = pop();

    if(p->tag != BOOL) {
        error("IFTE takes a boolean!\n");
    }

    return p->as_bool ? t : f;
}


Term_t* EQL() {
    Term_t* a = purify(pop());
    Term_t* b = purify(pop());
    Term_t* k = pop();

    if(a->tag != INT || b->tag != INT) {
        error("EQ takes two integer!\n");
    }

    return App(k,Bool(a->as_int == b->as_int));

}


Term_t* FLIP() {
    Term_t* x = purify(pop());
    Term_t* k = pop();
    if(x == NULL) {
        error("expect a data and a continuation!\n");
    }

    return App(k,x);
}

Term_t* iADD() {
    Term_t* a = purify(pop());
    Term_t* b = purify(pop());
    Term_t* k = pop();

    if(a->tag != INT || b->tag != INT) {
        error("ADD_i takes two integer!\n");
    }

    return App(k,Int(a->as_int + b->as_int));
}

Term_t* iSUB() {
    Term_t* a = purify(pop());
    Term_t* b = purify(pop());
    Term_t* k = pop();

    if(a == NULL || b == NULL) {
        error("expect two data and a continuation!\n");
    }

    if(a->tag != INT || b->tag != INT) {
        error("iSUB takes two integer!\n");
    }

    return App(k,Int(a->as_int - b->as_int));
}

Term_t* iPRINT() {
    Term_t* a = pop();
    Term_t* k = pop();
    
    if(a == NULL) {
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

    return App(k,Cons(h,t));
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

    return App(k,p->t1);
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

    return App(k,p->t2);
}