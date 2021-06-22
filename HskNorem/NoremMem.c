#include "Norem.h"

char** tag_base;

Term_t** stack;
Term_t** stack_base;
Term_t** stack_ceil;

Term_t* heap;
Term_t* heap_base;
Term_t* heap_ceil;

Term_t** trash;
Term_t** trash_base;
Term_t** trash_ceil;

void new_stack() {
    stack_base = malloc(STACK_SIZE * sizeof(Term_t**));
    stack_ceil = stack_base + STACK_SIZE;
    stack = stack_ceil;
}

inline void push(Term_t* term) {
    *stack-- = term;
}

inline Term_t* pop() {
    stack ++;
    return *stack;
}

atomic_flag trash_lock = ATOMIC_FLAG_INIT;

void trash_init() {
    trash_base = malloc(TRASH_SIZE * sizeof(Term_t**));
    trash_ceil = trash_base + TRASH_SIZE;
    trash = trash_base;
}

void run_gc() {
    // TODO : mark-sweep garbage collection
    printf("oops! gc not finished yet!\n");
    abort();
}

void new_heap() {
    heap_base = malloc(HEAP_SIZE * sizeof(Term_t));
    heap_ceil = heap_base + HEAP_SIZE;
    heap = heap_base;
}

void dump_heap() {
    if(heap == NULL) {
        return;
    }
    while(atomic_flag_test_and_set(&trash_lock)){} // spinlock
    if(trash >= trash_ceil) {
        run_gc();
    }
    *trash++ = heap;
    atomic_flag_clear(&trash_lock); // unlock
}

void mem_init() {
    tag_base = malloc(TAG_SIZE * sizeof(char*));
    trash_init();
    heap = NULL;
    new_heap();
    new_stack();
}

inline Term_t* new_term() {
    if(heap >= heap_ceil) {
        dump_heap();
        new_heap();
    }
    return heap++;
}

Term_t* new_app(Term_t* fun, Term_t* arg) {
    Term_t* term = new_term();
    term->fun = fun;
    term->arg = arg;
    return term;
}

Term_t* new_int(int64_t value) {
    Term_t* term = new_term();
    term->tag = tag_base + tag_int;
    term->as_int = value;
    return term;
}

Term_t* new_func(void* func) {
    Term_t* term = new_term();
    term->tag = tag_base + tag_func;
    term->as_func = func;
    return term;
}

Term_t* new_link(char* name) {
    Term_t* term = new_term();
    term->tag = tag_base + tag_link;
    term->as_link = name;
    return term;
}