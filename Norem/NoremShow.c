#include "Norem.h"

bool is_list(Term_t* arg_term) {
    Term_t* term = arg_term;

    while(true) {
        switch(term->tag) {
            case CONS:
                term = term->t2;
                continue;
            case NIL:
                return true;
            default:
                return false;
        }
    }
}

void show_list(Term_t* list) {
    Term_t* term = list;
    bool first_time;

    switch(term->tag) {
        case CONS:
            first_time = true;
            printf("[");
            while(true) {
                switch(term->tag) {
                    case CONS:
                        if(first_time) {
                            first_time = false;
                        } else {
                            printf(",");
                        }
                        show_term(term->t1);
                        term = term->t2;
                        continue;
                    case NIL:
                        printf("]");
                        return;
                    default:
                        error("show_list takes a list!\n");
                }
            }
        case NIL:
            printf("NIL");
            return;
        default:
            show_term(term);
    }
}

Term_t* purify(Term_t* term) {
    if(term == NULL) { return NULL; }

    switch(term->tag) {
        case ABS:
            return Abs(term->x,purify(term->t));
        case APP:
            if(term->t1->tag == ENV) {
                return purify(apply_env(term->t1,term->t2));
            } else {
                return App(purify(term->t1),purify(term->t2));
            }
        default:
            return term;
    }
}

void show_term(Term_t* arg_term) {
    Term_t* term = arg_term;

    if(term == NULL) {
        printf("NULL!\n");
        return;
    }

    //printf("show ");
    //printf("tag=%d,ptr=%o,org=%o\n",term->tag,term,arg_term);
    switch(term->tag) {
        case VAR:
            if(term->x->key == "`") {

            }
            printf(term->x->key);
            return;
        case ABS:
            printf("λ%s.",term->x->key);
            show_term(term->t);
            return;
        case APP:
            printf("(");
            show_term(term->t1);
            printf(" ");
            show_term(term->t2);
            printf(")");
            return;
        case ENV:
            printf("%s->",term->x->key);
            show_term(term->t);
            return;
        case CONS:
            if(is_list(term)) {
                show_list(term);
            } else {
                printf("(");
                show_term(term->t1);
                printf(",");
                show_term(term->t2);
                printf(")");
            }
            return;
        case INT:
            printf("%ld",term->as_int);
            return;
        case REAL:
            printf("%lf",term->as_real);
            return;
        case CHAR:
            printf("%c",term->as_char);
            return;
        case NIL:
            printf("NIL");
            return;
        case FUNC:
            printf("func");
            return;
        case SYMB:
            printf(term->as_symb->key);
        case DEBUG:
            printf("'");
            show_term(term->t);
    }
}