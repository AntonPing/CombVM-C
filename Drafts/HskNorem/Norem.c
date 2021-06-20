#include "Norem.h"

Term_t* eval(Term_t* term) {
    bool untouched = true;

    Term_t* with = term;
    Term_t* db[8];
    Term_t* sb[64];
    Term_t** dp = db;
    Term_t** sp = sb;

    // expand stack
    NEXT:
    if(is_app(with)) {
        push(with->arg);
        with = with->fun;
        goto NEXT;
    }

    switch(with->tag - tag_base) {
        case INT:
        case REAL:
            printf("ERROR, eval data\n");
            return NULL;
        case S:
            if(args < 3) {
                return NULL;
            }
            Term_t* f = stack[1];
            Term_t* g = stack[2];
            Term_t* x = stack[3];
            stack += 3;
            push(new_app(g,x));
            push(x);
            printf(",ok!\n");
            return f;//new_app(f,x),new_app(g,x);


            ret = with->as_func(frame - stack);
            if(ret == NULL) {
                if(untouched) {
                    // identity sincke it's untouched
                    return term;
                } else {
                    // stack wrapping
                    while(stack >= frame) {
                        with = new_app(with,pop());
                    }
                    return with;
                }
            } else {
                untouched = false;
                with = ret;
                goto NEXT;
            }
        case tag_int: //printf("2\n");
        case tag_real: //printf("3\n");
            if(stack == frame) {
                return with;
            } else {
                printf("Data can't be func!\n");
                abort();
            }
    }
}

int main() {
    mem_init();
    dict_init();
    eval_name("Test");
    return 0;
}