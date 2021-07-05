#include "Norem.h"

// TODO: better heap
static Term_t heap_base[2048];
static Term_t* heap_ceil = &heap_base[2047];
static Term_t* heap_ptr = &heap_base[0];

Term_t* alloc_term() {
    if(heap_ptr > heap_ceil) {
        return NULL; // TODO: gc
    } else {
        return heap_ptr++;
    }
}

Term_t* new_app(Term_t* t1, Term_t* t2) {
    Term_t* term = alloc_term();
    term->t1 = t1;
    term->t2 = t2;
    return term;
}

// exactly same as "new_app"
Term_t* new_cons(Term_t* t1, Term_t* t2) {
    Term_t* term = alloc_term();
    term->t1 = t1;
    term->t2 = t2;
    return term;
}

Term_t* new_int(int_t value) {
    Term_t* term = alloc_term();
    term->tag = &tags[INT];
    term->int_v = value;
    return term;
}

Term_t* new_real(real_t value) {
    Term_t* term = alloc_term();
    term->tag = &tags[REAL];
    term->real_v = value;
    return term;
}

Term_t* new_char(char_t value) {
    Term_t* term = alloc_term();
    term->tag = &tags[CHAR];
    term->char_v = value;
    return term;
}

Term_t* new_bool(bool_t value) {
    Term_t* term = alloc_term();
    term->tag = &tags[BOOL];
    term->bool_v = value;
    return term;
}

Term_t* new_symb(symb_t value) {
    Term_t* term = alloc_term();
    term->tag = &tags[SYMB];
    term->symb_v = value;
    return term;
}

Term_t* new_lamb(symb_t x, Term_t* t) {
    Term_t* term = alloc_term();
    term->tag = &tags[LAMB];
    Lambda_t* lamb = malloc(sizeof(Lambda_t));
    lamb->x = x;
    lamb->t = t;
    term->lamb_v = lamb;
    return term;
}