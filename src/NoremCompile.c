#include "Norem.h"

bool is_tag(Term_t* term) {
    return term >= &tags[0] && term < &tags[100];
}
bool is_singleton(Term_t* term) {
    return term >= &tags[100] && term <= &tags[255];
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
        if(term->lamb_v->x == x) {
            return false;
        } else {
            return is_free_in(x,term->lamb_v->t);
        }
    } else if(is_app(term)) {
        return is_free_in(x,term->t1) || is_free_in(x,term->t2);
    } else if(is_atom(term)) {
        return false;
    } 

    printf("impossible!\n");
    return false;
}

Term_t* term_compile(Term_t* term) {
    if(term == NULL) {
        puts("can't compile NULL!");
        return NULL;
    } else if(is_var(term)) {
        return term;
    } else if(is_lambda(term)) {
        Lambda_t* lamb = term->lamb_v;
        if(is_free_in(lamb->x,lamb->t)) {
            if(is_var(lamb->t)) {
                // we have term in form \x.y, and x is free in y
                // obviously x == y, it must be \x.x
                assert(lamb->x == lamb->t->symb_v);
                // T[\x.x] -> I
                return &tags[I];
            } else if(is_lambda(lamb->t)) {
                // T[\x.\y.E] -> T[\x.T[\y.E]]
                Term_t* t2 = term_compile(lamb->t);
                return term_compile(new_lamb(lamb->x,t2));
            } else if(is_app(lamb->t)) {
                // T[\x.(E1 E2)] -> (S T[\x.E1] T[\x.E2])
                Term_t* t1 = lamb->t->t1;
                Term_t* t2 = lamb->t->t2;
                return new_app(new_app(&tags[S],
                            term_compile(new_lamb(lamb->x,t1))),
                            term_compile(new_lamb(lamb->x,t2)));
            }
        } else {
            // T[\x.E] -> (K T[E]), if x is not free in E
            return new_app(&tags[K],lamb->t);
        }
    } else if(is_app(term)) { // for application
        // T[(E1 E2)] -> (T[E1] T[E2])
        return new_app(term_compile(term->t1),
                        term_compile(term->t2));
    } else if(is_atom(term)) {
        // Non-Lambda Atom
        return term;
    } 
    printf("impossible!\n");
    return NULL;
}

typedef struct Symb_List_t {
    symb_t this;
    struct Symb_List_t* next;
} Symb_List_t;

bool symb_list_lookup(symb_t symb, Symb_List_t* list) {
    Symb_List_t* look = list;
    while(true) {
        if(look == NULL) {
            return false;
        } else if(look->this == symb) {
            return true;
        } else {
            look = look->next;
        }
    }
}

Term_t* term_link_helper(Term_t* term, Symb_List_t* list) {
    //show_term(term); printf("\n");
    if(term == NULL) {
        PANIC("NULL!\n");
    } else if(is_var(term)) {
        if(symb_list_lookup(term->symb_v,list)) {
            return term;
        } else {
            Dict_t* dict = dict_get(term->symb_v);
            if(dict == NULL) {
                LOG("can't find definition %s\n",term->symb_v);
                return NULL;
            } else {
                if(dict->linked == NULL) {
                    dict->linked = term_link(dict->raw);
                }
                if(dict->compiled == NULL) {
                    dict->compiled = term_compile(dict->linked);
                }
                return dict->compiled;
            }
        }
    } else if(is_lambda(term)) {
        Lambda_t* lamb = term->lamb_v;
        Symb_List_t new_list = { lamb->x, list };
        return new_lamb(lamb->x, term_link_helper(lamb->t, &new_list));
    } else if(is_app(term)) {
        return new_app(term_link_helper(term->t1, list),
                       term_link_helper(term->t2, list));
    } else {
        return term;
    }
}

Term_t* term_link(Term_t* term) {
    return term_link_helper(term, NULL);
}


