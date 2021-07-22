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
        term = raw_app(term,*sp); \
    } \
    with = term; \
    goto eval_loop; \
} while(0)

#define WITH(x) \
    with = x

#define PUSH(x) \
    *sp++ = x; \
    assert(sp <= &stack[15])

#define NEXT() \
    goto eval_loop

#define ARG_1(x) \
    if(sp < &stack[1]) { \
        DOWN_REWIND(); \
    } else { \
        x = *(--sp); \
    }

#define ARG_2(x,y) \
    if(sp < &stack[2]) { \
        DOWN_REWIND(); \
    } else { \
        x = *(--sp); \
        y = *(--sp); \
    }

#define ARG_3(x,y,z) \
    if(sp < &stack[3]) { \
        DOWN_REWIND(); \
    } else { \
        x = *(--sp); \
        y = *(--sp); \
        z = *(--sp); \
    }

#define EVAL(x,y) \
    y = eval(x)

#define INC(x) \
    gc_refer(x)

#define DEC(x) \
    gc_deref(x)

#define SHOW_STACK() do{ \
    show_term(with); \
    Term_t** ptr = sp; \
    while(ptr > &stack[0]) { \
        printf(", "); \
        show_term(*(--ptr)); \
    }\
    printf("\n"); \
} while(0)


#define INT_BINOP(result) \
    ARG_2(x,y); \
    EVAL(x,a); \
    EVAL(y,b); \
    assert(a->tag == INT); \
    assert(b->tag == INT); \
    WITH(result); \
    DEC(x); DEC(y); NEXT()


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
        //show_term(with);
        //printf("\n");
        SHOW_STACK();
    #endif

    assert(with != NULL);
    switch(with->tag) {
        Term_t *x,*y,*z,*a,*b;
        case APP:
            Term_t* t1 = with->t1;
            Term_t* t2 = with->t2;
            if(with->rc == 0) {
                free_term(with);
            } else {
                with->rc --;
            }
            PUSH(t2);
            WITH(t1);
            NEXT();
        case I:
            ARG_1(x); // x--
            WITH(x); // x++
            NEXT(); 
        case K:
            ARG_2(x,y); // x-- y--
            WITH(x); // x++
            DEC(y); NEXT(); // y=-1
        case S:
            ARG_3(x,y,z); // x-- y-- z--
            PUSH(raw_app(y,z)); // y++ z++
            PUSH(z); // z++
            WITH(x); // x++
            INC(z); NEXT(); // z=+1
        case ADDI:
            INT_BINOP(new_int(a->int_v + b->int_v));
        case SUBI:
            INT_BINOP(new_int(a->int_v - b->int_v));
        case MULI:
            INT_BINOP(new_int(a->int_v * b->int_v));
        case DIVI:
            INT_BINOP(new_int(a->int_v / b->int_v));

        case NEGI:
            POP_1(x);
            EVAL(x);
            assert(x->tag == INT);
            NEXT(new_int(x->int_v * -1));
        
        case IF:
            POP_3(x,y,z); EVAL(x);
            assert(x->tag == BOOL);
            WITH(x->bool_v ? y : z);
        case NOT:
            POP_1(x); EVAL(x);
            assert(x->tag == BOOL);
            NEXT(new_bool(!x->bool_v));
        case EQL:
            POP_2(x,y); EVAL(x); EVAL(y);
            assert(x->tag == INT);
            assert(y->tag == INT);
            NEXT(new_bool(x->int_v == y->int_v));
        case GRT:
            POP_2(x,y); EVAL(x); EVAL(y);
            assert(x->tag == INT);
            assert(y->tag == INT);
            NEXT(new_bool(x->int_v > y->int_v));
        case LSS:
            POP_2(x,y); EVAL(x); EVAL(y);
            assert(x->tag == INT);
            assert(y->tag == INT);
            NEXT(new_bool(x->int_v < y->int_v));
        case PRINTI:
            POP_2(x,y); EVAL(x);
            assert(x->tag == INT);
            printf("printi %ld\n", x->int_v);
            NEXT(y);
        case EXIT:
            exit(0);

        case SYMB: {
            Dict_t* dict = dict_get(with->symb_v);
            if(dict != NULL) {
                assert(dict->compiled != NULL);
                WITH(dict->compiled);
                NEXT();
            } else {
                PANIC("undefined symbol!\n");
            }
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
            PANIC("Unknown tag when eval term!\n");
    }
}
