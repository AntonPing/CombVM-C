#include "Norem.h"

Term_t* heap_base;
Term_t* heap_ceil;
Term_t* heap_ptr;

Term_t** stack_base;
Term_t** stack_ceil;
Term_t** stack_ptr;

void error(char* msg) {
    printf("error: %s\n",msg);
    abort();
}

void push(Term_t* arg_term) {
    *stack_ptr-- = arg_term;
    if(stack_ptr < stack_base) {
        error("stack overflow!");
    }
    return;
}

Term_t* pop() {
    if(stack_ptr == stack_ceil) {
        error("insuffient arguments");
        return NULL;
    } else {
        stack_ptr ++;
        return *stack_ptr;
    }
}

Term_t* stack_rewind(Term_t* arg_term) {
    Term_t** old_stack = stack_ptr;
    Term_t* term = arg_term;
    while(stack_ptr < stack_ceil) {
        stack_ptr++;
        term = App(term,*stack_ptr);
    }
    stack_ptr = old_stack;
    return term;
}

void heap_init() {
    assert(sizeof(Term_t) == 24);
    heap_base = malloc(HEAP_SIZE * sizeof(Term_t));
    printf("heap_init: %o\n",heap_base);
    heap_ceil = heap_base + HEAP_SIZE;
    heap_ptr = heap_base;

    assert(((uint64_t)heap_base & 0b111) == 0);
    assert(((uint64_t)heap_ptr & 0b111) == 0);
}

void stack_init() {
    stack_base = malloc(STACK_SIZE * sizeof(Term_t*));
    printf("stack_init: %o\n",stack_base);
    stack_ceil = stack_base + STACK_SIZE;
    stack_ptr = stack_ceil; // the stack grows from top to bottom
}


Term_t* new_term() {
    if(heap_ptr >= heap_ceil) {
        error("Oops, gc is not ready!\n");
    }
    //assert(((uint64_t)heap_ptr & 0b111) == 0);
    return heap_ptr++;
}

Term_t* Var(Symb_t* x) {
    Term_t* term = new_term();
    term->tag = VAR;
    term->x = x;
    return term;
}

Term_t* Abs(Symb_t* x, Term_t* t) {
    Term_t* term = new_term();
    term->tag = ABS;
    term->x = x;
    term->t = t;
    return term;
}

Term_t* App(Term_t* t1, Term_t* t2) {
    Term_t* term = new_term();
    term->tag = APP;
    term->t1 = t1;
    term->t2 = t2;
    return term;
}

Term_t* Env(Symb_t* x, Term_t* t) {
    Term_t* term = new_term();
    term->tag = ENV;
    term->x = x;
    term->t = t;
    return term;
}

Term_t* Int(int64_t n) {
    Term_t* term = new_term();
    term->tag = INT;
    term->as_int = n;
    return term;
}

Term_t* Real(double x) {
    Term_t* term = new_term();
    term->tag = REAL;
    term->as_real = x;
    return term;
}

Term_t* Symb(Symb_t* s) {
    Term_t* term = new_term();
    term->tag = SYMB;
    term->as_symb = s;
    return term;
}

Term_t* Cons(Term_t* h, Term_t* t) {
    Term_t* term = new_term();
    term->tag = CONS;
    term->t1 = h;
    term->t2 = t;
    return term;
}

Term_t* Nil() {
    Term_t* term = new_term();
    term->tag = NIL;
    return term;
}

Term_t* Debug(Term_t* t) {
    Term_t* term = new_term();
    term->tag = DEBUG;
    term->t = t;
    return term;
}

Term_t* Bool(bool p) {
    Term_t* term = new_term();
    term->tag = BOOL;
    term->as_bool = p;
    return term;
}