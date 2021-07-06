#include "Norem.h"

bool is_tag(Term_t* term) {
    return term >= &tags[0] && term < &tags[100];
}
bool is_singleton(Term_t* term) {
    return term >= &tags[100] && term <= &tags[256];
}
bool is_atom(Term_t* term) {
    return is_singleton(term) || is_tag(term->tag);
}
bool is_app(Term_t* term) {
    return !is_singleton(term) && !is_tag(term->tag);
}
bool is_var(Term_t* term) {
    return term->tag == &tags[SYMB];
}
bool is_lambda(Term_t* term) {
    return term->tag == &tags[LAMB];
}

bool is_free_in(symb_t x, Term_t* term) {
    if(is_var(term)) {
        return x == term->symb_v;
    } else if(is_lambda(term)) {
        Lambda_t* lamb = term->lamb_v;
        if(lamb->x == x) {
            return false;
        } else {
            return is_free_in(x,lamb->t);
        }
    } else if(is_app(term)) {
        return is_free_in(x,term->t1) || is_free_in(x,term->t2);
    } else if(is_atom(term)) {
        return false;
    } 

    perror("impossible!\n");
    return false;
}

static int ii = 0;

Term_t* ski_compile(Term_t* term) {
    show_term(term); printf("\n");
    
    if(ii >= 100) {
        perror("impossible!\n");
    } else {
        ii ++;
    }

    if(is_var(term)) {
        return term;
    } else if(is_lambda(term)) {
        Lambda_t* lamb = term->lamb_v;
        if(is_free_in(lamb->x,lamb->t)) {
            printf("here free\n");
            if(is_var(lamb->t)) {
                // we have term in form \x.y, and x is free in y
                // obviously x == y, it must be \x.x
                assert(lamb->x == lamb->t->symb_v);
                // T[\x.x] -> I
                return &tags[I];
            } else if(is_lambda(lamb->t)) {
                // T[\x.\y.E] -> T[\x.T[\y.E]]
                Term_t* t2 = ski_compile(lamb->t);
                return ski_compile(new_lamb(lamb->x,t2));
            } else if(is_app(lamb->t)) {
                // T[\x.(E1 E2)] -> (S T[\x.E1] T[\x.E2])
                Term_t* t1 = lamb->t->t1;
                Term_t* t2 = lamb->t->t2;
                return new_app(new_app(&tags[S],
                            ski_compile(new_lamb(lamb->x,t1))),
                            ski_compile(new_lamb(lamb->x,t2)));
            }
        } else {
            printf("here not free\n");
            // T[\x.E] -> (K T[E]), if x is not free in E
            return new_app(&tags[K],lamb->t);
        }
    } else if(is_app(term)) { // for application
        printf("here app\n");
        // T[(E1 E2)] -> (T[E1] T[E2])
        return new_app(ski_compile(term->t1),
                        ski_compile(term->t2));
    } else if(is_atom(term)) {
        // Non-Lambda Atom
        printf("here nonlambda atom\n");
        return term;
    } 
    perror("impossible!\n");
    return NULL;
}