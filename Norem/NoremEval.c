#include "Norem.h"

Term_t* apply_env(Term_t* env, Term_t* arg_term);

Term_t* eval(Term_t* arg_term) {
    Term_t* term = arg_term;
    Term_t* for_test;

    eval_loop:
    for_test = stack_rewind(term);
    //printf("t:");
    //show_term(for_test);
    //printf("\n");
    printf("p:");
    show_term(purify(for_test));
    printf("\n");

    //printf("tag=%d\n",tag);
    switch(term->tag) {
        case VAR:
            switch(term->x->state) {
                case UNDEF:
                    return stack_rewind(term);
                case BASIC:
                    term = term->x->fn();
                    goto eval_loop;
                case LINK:
                    term = term->x->value;
                    goto eval_loop;
            }
        case ABS: {
            Term_t* term2 = pop();
            if(term2 == NULL) {
                return term;
            } else {
                push(term->t);
                term = _Env(term->x,term2);
                goto eval_loop;
            }
        }
        case APP: {
            push(term->t2);
            term = term->t1;
            goto eval_loop;
        }
        case ENV: {
            Term_t* term2 = pop();
            if(term2 == NULL) {
                return term;
            } else {
                term = apply_env(term,term2);
                goto eval_loop;
            }
        }
        default:
            return stack_rewind(term);

    }
}

Term_t* apply_env(Term_t* env, Term_t* arg_term) {
    Term_t* term = arg_term;
    //printf("apply!\n");
    //show_term(arg_term);
    start:
    //printf("tag=%d\n",tag);
    switch(term->tag) {
        case VAR:
            if(term->x == env->x) {
                return env->t;
            } else {
                return term;
            }
        case ABS: {
            if(term->x == env->x) {
                return term;
            } else {
                return _Abs(term->x,_App(env,term->t));
            }
        }
        case APP: {
            if(term->t1->tag == ENV) {
                term = apply_env(term->t1,term->t2);
                goto start;
            } else {
                return _App(_App(env,term->t1),_App(env,term->t2));
            }
        }
        case ENV: {
            error("Env on Env!");
            return NULL;
        }
        default:
            return term;
    }
}

