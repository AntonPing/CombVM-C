#include "Norem.h"


bool is_lambda(Term_t* term) {
    return term->tag == &tags[LAMB];
}

bool is_free_in(symb_t x, Term_t* term) {
    if(is_atom(term)) {
        return false;
    } else if(is_app(term)) {
        return is_free_in(term->t1) || is_free_in(term->t2);
    } else if(is_lambda(term)) {
        Lambda_t* lamb = term->lamb_v;
        if(lamb->x == x) {
            return false;
        } else {
            return is_free_in(x,lamb->t);
        }
    } else {
        perror("impossible!\n");
    }
}


Term_t* ski_compile(Term_t* term) {
    if(is_atom(term)) {
        return term;
    } else if(is_app(term)) {
        return new_app(
            ski_compile(term->t1),
            ski_compile(term->t2));
    } else if(is_lambda(term)) {
        Lambda_t* lamb = term->lamb_v;
        if(is_free_in(lamb->x,lamb->t)) {
            if(lamb->t->tag == &tags[SYMB]) {
                assert(lamb->x == lamb->t->symb_v);
                return &tags[I];
            } else if(lamb->t->tag == &tags[LAMB]) {
                Term_t* term2 = ski_compile(lamb->t);
                Term_t* term3 = new_lamb(lamb->x,term2);
                return ski_compile(term3);
            } else if(is_app(lamb->t)) {
                // TODO
            }
        } else {
            return new_app(&tags[K],lamb->t);
        }
    }
}