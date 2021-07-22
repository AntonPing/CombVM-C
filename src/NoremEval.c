#include "Norem.h"
/*
#define DOWN_REWIND() \
    while(sp != &stack[0]) {} \
        sp --; \
        with = new_app(with,*sp); \
    } \
    return with

#define UP_REWIND(x) \
    sp = &stack[13]; \
    with = eval(new_app(new_app(new_app( \
        x,stack[15]),stack[14]),stack[13]));
*/

#define DOWN_REWIND() do{ \
    LOG("down rewind\n"); \
    Term_t* term = *sp; \
    while(sp > &stack[0]) { \
        sp --; \
        term = new_app(term,*sp); \
    } \
    NEXT(term); \
} while(0)

#define NEXT(x) \
    with = x; \
    goto eval_loop

#define PUSH(x) \
    *sp++ = x; \
    assert(sp <= &stack[15])

#define POP_1(x) \
    if(sp < &stack[1]) { \
        DOWN_REWIND(); \
    } else { \
        x = *(--sp); \
    }

#define POP_2(x,y) \
    if(sp < &stack[2]) { \
        DOWN_REWIND(); \
    } else { \
        x = *(--sp); \
        y = *(--sp); \
    }

#define POP_3(x,y,z) \
    if(sp < &stack[3]) { \
        DOWN_REWIND(); \
    } else { \
        x = *(--sp); \
        y = *(--sp); \
        z = *(--sp); \
    }

#define SHOW_STACK() do{ \
    show_term(with); \
    Term_t** ptr = sp; \
    while(ptr > &stack[0]) { \
        printf(", "); \
        show_term(*(--ptr)); \
    }\
    printf("\n"); \
} while(0)


#define EVAL(x) \
    x = eval(x)

Term_t* eval(Term_t* term) {
    Term_t* stack[16];
    Term_t** sp = &stack[0];
    Term_t* with = term;

    #ifdef DEBUG
        printf("eval call: ");
        show_term(with);
        printf("\n");
    #endif

    eval_loop:
    #ifdef DEBUG
        show_term(with);
        printf("\n");
        //SHOW_STACK();
    #endif

    if(with == NULL) {
        return NULL;
    } else if(is_singleton(with)) {
        switch(with - &tags[0]) {
            Term_t *x,*y,*z;//,*a,*b;
            case I:
                POP_1(x);
                NEXT(x);
            case K:
                POP_2(x,y);
                NEXT(x);
            case S:
                POP_3(x,y,z);
                PUSH(new_app(y,z));
                PUSH(z);
                NEXT(x);
            case ADDI:
                POP_2(x,y); EVAL(x); EVAL(y);
                assert(x->tag == &tags[INT]);
                assert(y->tag == &tags[INT]);
                NEXT(new_int(x->int_v + y->int_v));
            case SUBI:
                POP_2(x,y); EVAL(x); EVAL(y);
                assert(x->tag == &tags[INT]);
                assert(y->tag == &tags[INT]);
                NEXT(new_int(x->int_v - y->int_v));
            case MULI:
                POP_2(x,y); EVAL(x); EVAL(y);
                assert(x->tag == &tags[INT]);
                assert(y->tag == &tags[INT]);
                NEXT(new_int(x->int_v * y->int_v));
            case DIVI:
                POP_2(x,y); EVAL(x); EVAL(y);
                assert(x->tag == &tags[INT]);
                assert(y->tag == &tags[INT]);
                NEXT(new_int(x->int_v / y->int_v));
            case NEGI:
                POP_1(x); EVAL(x);
                assert(x->tag == &tags[INT]);
                NEXT(new_int(x->int_v * -1));
            case IF:
                POP_3(x,y,z); EVAL(x);
                assert(x->tag == &tags[BOOL]);
                NEXT(x->bool_v ? y : z);
            case NOT:
                POP_1(x); EVAL(x);
                assert(x->tag == &tags[BOOL]);
                NEXT(new_bool(!x->bool_v));
            case EQL:
                POP_2(x,y); EVAL(x); EVAL(y);
                assert(x->tag == &tags[INT]);
                assert(y->tag == &tags[INT]);
                NEXT(new_bool(x->int_v == y->int_v));
            case GRT:
                POP_2(x,y); EVAL(x); EVAL(y);
                assert(x->tag == &tags[INT]);
                assert(y->tag == &tags[INT]);
                NEXT(new_bool(x->int_v > y->int_v));
            case LSS:
                POP_2(x,y); EVAL(x); EVAL(y);
                assert(x->tag == &tags[INT]);
                assert(y->tag == &tags[INT]);
                NEXT(new_bool(x->int_v < y->int_v));
            case PRINTI:
                POP_2(x,y); EVAL(x);
                assert(x->tag == &tags[INT]);
                printf("printi %ld\n", x->int_v);
                NEXT(y);
            case EXIT:
                exit(0);
            default:
                show_term(with); printf("\n");
                PANIC("unknown singleton!\n");
        }
    } else if(is_tag(with->tag)) {
        switch(with->tag - &tags[0]) {
            Dict_t* dict;
            case SYMB:
                dict = dict_get(with->symb_v);
                if(dict != NULL) {
                    assert(dict->compiled != NULL);
                    NEXT(dict->compiled);
                } else {
                    PANIC("undefined symbol!\n");
                }
            case INT:
            case REAL:
            case CHAR:
            case BOOL:
                if(sp == &stack[0]) {
                    #ifdef DEBUG
                        printf("return value: ");
                        show_term(with);
                        printf("\n");
                    #endif
                    return with;
                } else {
                    show_term(with); printf("\n");
                    PANIC("basic data can't be function!\n");
                }
            default:
                PANIC("TODO");
        }
    } else { // is_app(with)
        PUSH(with->t2);
        NEXT(with->t1);
    }
}
