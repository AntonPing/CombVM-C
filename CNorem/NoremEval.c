#include "Norem.h"

Term_t* apply_env(Term_t* env, Term_t* arg_term);

Term_t* eval(Term_t* arg_term) {
    Term_t* term = arg_term;

    eval_loop:
    if(term == NULL) {
        error("EVAL NULL");
        return NULL;
    }

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
                term = Env(term->x,term2);
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
        case DEBUG: {
            Term_t* debug = purify(stack_rewind(term));
            printf("DEBUG: ");
            show_term(debug);
            printf("\n");
            term = term->t;
            goto eval_loop;
        }
        case INT:
        case REAL:
        case CHAR:
        case BOOL:
        case STR:
        case DATA: {
            Term_t* term2 = pop();
            if(term2 == NULL) {
                return term;
            } else {
                push(term);
                term = term2;
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
                return Abs(term->x,App(env,term->t));
            }
        }
        case APP: {
            if(term->t1->tag == ENV) {
                term = apply_env(term->t1,term->t2);
                goto start;
            } else {
                return App(App(env,term->t1),App(env,term->t2));
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

