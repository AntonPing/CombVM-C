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

#define DOWN_REWIND() \
    printf("down_rewind\n"); \
    sp = sp

#define NEXT(x) \
    with = x; \
    goto eval_loop

#define PUSH(x) \
    *sp++ = x

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

Term_t* eval(Term_t* term) {
    Term_t* stack[16];
    Term_t** sp = &stack[0];
    Term_t* with = term;

    DBG("eval call\n");

    eval_loop:
    #ifdef DEBUG
        SHOW_STACK();
    #endif

    if(is_singleton(with)) {
        switch(with - &tags[0]) {
            Term_t *x,*y,*z,*a,*b;
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
                POP_2(x,y);
                a = eval(x);
                b = eval(y);
                assert(a->tag == &tags[INT]);
                assert(b->tag == &tags[INT]);
                z = new_int(a->int_v + b->int_v);
                NEXT(z);
            case PRINTI:
                POP_2(x,y);
                a = eval(x);
                assert(a->tag == &tags[INT]);
                printf("printi %ld\n", a->int_v);
                NEXT(y);
            case DEFINE:
                PANIC("TODO");
            case EXIT:
                exit(0);
            default:
                show_term(with); printf("\n");
                PANIC("unknown singleton!\n");
        }
    } else if(is_tag(with->tag)) {
        switch(with->tag - &tags[0]) {
            case SYMB:
                Dict_t* dict = dict_get(with->symb_v);
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
                    DBG("return value\n");
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
