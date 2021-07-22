#include "Norem.h"

//Type_t types[256];

Term_t sing[256];

#define POOL_SIZE 65536
static Term_t term_pool[POOL_SIZE];
static Term_t* heap_base[POOL_SIZE];
static Term_t* *heap_ceil;
static Term_t* *heap_ptr;


void heap_init() {
    LOG("start, init heap");
    for(int i=0; i<POOL_SIZE; i++) {
        heap_base[i].rc = 0;
        heap_base[i] = &term_pool[i];
    }
    heap_ceil = &heap_base[POOL_SIZE - 1];
    heap_ptr = &heap_base[POOL_SIZE - 1]
    LOG("init singleton");
    int i = 0;
    sing[S].tag = S
    sing[K].tag = K
    sing[I].tag = I
    LOG("finish");
}

void free_term(Term_t* term) {
    assert(heap_ptr >= heap_base && heap_ptr <= heap_ceil);
    if(heap_ptr == heap_ceil) {
        PANIC("heap overflow, this should never heappen!\n");
    } else {
        heap_ptr ++;
        *heap_ptr = term;
    }
}

Term_t* alloc_term() {
    assert(heap_ptr >= heap_base && heap_ptr <= heap_ceil);
    if(heap_ptr == heap_base) {
        PANIC("heap all used!\n");
    } else {
        assert(*heap_ptr->rc == 0);
        return *heap_ptr--;
    }
}

void gc_free(Term_t* term) {
    switch(term->tag) {
        case APP:
            gc_defer(term->t1);
            gc_defer(term->t2);
        case LAMB:
            gc_defer(term->t);
        default: {
            // Do Nothing
        }
    }
    free_term(term);
}

Term_t* gc_refer(Term_t* term) {
    term->rc ++;
    return term;
}

void gc_defer(Term_t* term) {
    term->rc --;
    assert(term->rc >= 0);
    if(term->rc == 0) {
        gc_free(term);
    }
}

Term_t* new_app(Term_t* t1, Term_t* t2) {
    Term_t* term = alloc_term();
    term->tag = APP;
    term->t1 = gc_refer(t1);
    term->t2 = gc_refer(t2);
    return term;
}

Term_t* new_int(int_t value) {
    Term_t* term = alloc_term();
    term->tag = INT;
    term->int_v = value;
    return term;
}

/*
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
*/

Term_t* new_bool(bool_t value) {
    Term_t* term = alloc_term();
    term->tag = BOOL;
    term->bool_v = value;
    return term;
}

Term_t* new_symb(symb_t value) {
    Term_t* term = alloc_term();
    term->tag = SYMB;
    term->symb_v = value;
    return term;
}

Term_t* new_lamb(symb_t x, Term_t* t) {
    Term_t* term = alloc_term();
    term->tag = LAMB;
    term->x = x;
    term->t = gc_refer(t);
    return term;
}