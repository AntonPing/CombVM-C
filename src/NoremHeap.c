#include "Norem.h"

//Type_t types[256];

Term_t sing[256];

#define POOL_SIZE 2048
static Term_t heap_base_a[POOL_SIZE];
static Term_t* heap_ceil_a = &heap_base_a[POOL_SIZE - 1];
static Term_t heap_base_b[POOL_SIZE];
static Term_t* heap_ceil_b = &heap_base_b[POOL_SIZE - 1];
static bool from_a_to_b = true;

static Term_t* heap_ptr;

void heap_init() {
    LOG("start, init heap\n");
    heap_ptr = &heap_base_a[0];
    LOG("init singleton\n");
    for(int i=0; i<256; i++) {
        sing[i].tag = i;
    }
    LOG("finish\n");
}

/*
void free_term(Term_t* term) {
    PANIC("term free!\n");
    assert(heap_ptr >= heap_base && heap_ptr <= heap_ceil);
    if(heap_ptr == heap_ceil) {
        PANIC("heap overflow, this should never heappen!\n");
    } else {
        heap_ptr ++;
        *heap_ptr = term;
    }
}
*/

Term_t* copy_term(Term_t* term) {
    switch(term->tag) {
        case S:
        case K:
        case I:
        case B:
        case C:
        case BS:
        case CP:
        case SP:
        case Y: 
        case E: 
        case ADDI:
        case SUBI:
        case MULI:
        case DIVI:
        case NEGI:
        case IF:
        case NOT:
        case EQL:
        case GRT:
        case LSS:
        case PRINTI:
        case EXIT:
        case NIL:
            return term; 
        case INT:
            return new_int(term->int_v);
        case REAL:
            return new_real(term->real_v);
        case CHAR:
            return new_char(term->char_v);
        case BOOL:
            return new_bool(term->bool_v);
        case SYMB:
            return new_symb(term->symb_v);
        case LAMB: 
            return new_lamb(term->x,
                copy_term(term->t));
        case APP: 
            return new_app(
                copy_term(term->t1),
                copy_term(term->t2));
        default:
            PANIC("unknown tag for copy-collect!\n");
    }
}

void run_gc() {
    if(from_a_to_b) {
        from_a_to_b = false;
        heap_ptr = heap_base_b;
    } else {
        from_a_to_b = true;
        heap_ptr = heap_base_a;
    }

    Task_t* *task_ptr = task_head + 1;
    while(task_ptr++ != task_tail) {
        if(task_ptr == task_queue_ceil + 1) {
            task_ptr = task_queue_base;
        }
        Task_t* task = *task_ptr;
        Term_t* *base = task->stack_base;
        Term_t* *sp = task->sp;
        task->ret = copy_term(task->ret);
        for(Term_t** ptr = base; ptr <= sp; ptr ++) {
            *ptr = copy_term(*ptr);
        }
    }
}


Term_t* alloc_term() {
    if(from_a_to_b) {
        assert(heap_ptr >= heap_base_a && heap_ptr <= heap_ceil_a);
        if(heap_ptr == heap_ceil_a) {
            run_gc();
            return alloc_term();
        } else {
            return heap_ptr++;
        }
    } else {
        assert(heap_ptr >= heap_base_b && heap_ptr <= heap_ceil_b);
        if(heap_ptr == heap_ceil_b) {
            run_gc();
            return alloc_term();
        } else {
            return heap_ptr++;
        }
    }
}


/*
void gc_free(Term_t* term) {
    assert(term != NULL);
    PANIC("gc free!\n");
    switch(term->tag) {
        case APP:
            gc_deref(term->t1);
            gc_deref(term->t2);
            break;
        case LAMB:
            gc_deref(term->t);
            break;
        default: {
            // Do Nothing
        }
    }
    free_term(term);
}

Term_t* gc_refer(Term_t* term) {
    PANIC("gc disabled!\n");
    assert(term != NULL);
    term->rc ++;
    return term;
}

void gc_deref(Term_t* term) {
    PANIC("gc disabled!\n");
    assert(term != NULL && term->rc >= 0);
    if(term->rc == 0) {
        gc_free(term);
    } else {
        term->rc --;
    }
}

*/

Term_t* new_app(Term_t* t1, Term_t* t2) {
    Term_t* term = alloc_term();
    term->tag = APP;
    term->t1 = t1;
    term->t2 = t2;//gc_refer(t2);
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
    term->t = t;
    return term;
}

Term_t* new_box() {
    Term_t* term = alloc_term();
    term->tag = BOX;
    return term;
}

/*
Term_t* new_thunk(Term_t* t) {
    Term_t* term = alloc_term();
    term->tag = THUNK;
    //term->thunk_v = t;
    // TODO
    return term;
}
*/