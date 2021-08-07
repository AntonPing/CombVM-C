#include "Norem.h"
#define HEAP_SIZE 1024
#define BUFFER_SIZE 512

Term_t heap_base_a[HEAP_SIZE];
Term_t* heap_ceil_a = &heap_base_a[HEAP_SIZE - BUFFER_SIZE];
Term_t heap_base_b[HEAP_SIZE];
Term_t* heap_ceil_b = &heap_base_b[HEAP_SIZE - BUFFER_SIZE];
bool from_a_to_b = true;

Term_t* heap_ptr;
Term_t sing[256];

void heap_init() {
    LOG("start, init heap\n");
    heap_ptr = &heap_base_a[0];
    LOG("init singleton\n");
    for(int i=0; i<256; i++) {
        sing[i].tag = i;
    }
    LOG("finish\n");
}

Term_t* copy_term(Term_t* term) {
    LOG("copy %p.", term);
    if(term == NULL) {
        return term;
    }
    if(term == &FRAME) {
        return term;
    }
    if(term == &HOLE) {
        return term;
    }

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

// from Norem.c
extern Dict_t* root;
void dict_gc() {
    Dict_t* ptr = root;
    while(ptr != NULL) {
        LOG("gc dict: %s\n",ptr->name);
        ptr->raw = copy_term(ptr->raw);
        ptr->linked = copy_term(ptr->linked);
        ptr->compiled = copy_term(ptr->compiled);
        ptr = ptr->next;
    }
}

void show_heap_info() {
    #ifdef DEBUG
        int used, cap;
        if(from_a_to_b) {
            used = heap_ptr - &heap_base_a[0];
            cap = &heap_base_a[HEAP_SIZE] - heap_ptr;
        } else {
            used = heap_ptr - &heap_base_b[0];
            cap = &heap_base_b[HEAP_SIZE] - heap_ptr;
        }
        LOG("heap info: used %d, cap %d.\n", used, cap);
        LOG("using %c heap, stop_the_world = %d, stopped num = %d.\n",
                    from_a_to_b ? 'A' : 'B', stop_the_world, stopped_num);
    #endif
}

extern Task_t* *task_queue_base;
extern Task_t* *task_queue_ceil;
extern Task_t* *task_head;
extern Task_t* *task_tail;

void run_gc() {
    // TODO: stop the world
    
    LOG("gc start!");
    show_heap_info();

    if(from_a_to_b) {
        from_a_to_b = false;
        heap_ptr = &heap_base_b[0];
    } else {
        from_a_to_b = true;
        heap_ptr = &heap_base_a[0];
    }
    Task_t** task_ptr = task_head + 1;

    LOG("head %p, tail %p, ptr %p\n", task_head, task_tail, task_ptr);
    
    while(task_ptr != task_tail) {
        LOG("collecting task...");
        Task_t* task = *task_ptr;
        Term_t* *base = task->stack_base;
        Term_t* *sp = task->sp;
        task->ret = copy_term(task->ret);
        for(Term_t** ptr = base; ptr <= sp; ptr ++) {
            *ptr = copy_term(*ptr);
        }
        assert(task_ptr >= task_queue_base && task_ptr <= task_queue_ceil);
        if(task_ptr == task_queue_ceil) {
            task_ptr = task_queue_base;
        } else {
            task_ptr ++;
        }
    }

    dict_gc();

    if(from_a_to_b) {
        for(int i=0; i<HEAP_SIZE; i++) {
            heap_base_b[i].tag = NIL;
            heap_base_b[i].t1 = NULL;
            heap_base_b[i].t2 = NULL;
        }
    } else {
        for(int i=0; i<HEAP_SIZE; i++) {
            heap_base_a[i].tag = NIL;
            heap_base_a[i].t1 = NULL;
            heap_base_a[i].t2 = NULL;
        }
    }

    LOG("gc over!");
    show_heap_info();
    sleep(5);
}

Term_t* alloc_term() {
    if(from_a_to_b) {
        assert(heap_ptr >= heap_base_a && heap_ptr < heap_base_a + HEAP_SIZE);
        if(heap_ptr >= heap_ceil_a) {
            stop_the_world = true;
        }
        return heap_ptr++;
    } else {
        assert(heap_ptr >= heap_base_b && heap_ptr < heap_base_b + HEAP_SIZE);
        if(heap_ptr >= heap_ceil_b) {
            stop_the_world = true;
        }
        return heap_ptr++;
    }
}


Term_t* new_app(Term_t* t1, Term_t* t2) {
    Term_t* term = alloc_term();
    term->tag = APP;
    term->t1 = t1;
    term->t2 = t2;
    return term;
}

Term_t* new_int(int_t value) {
    Term_t* term = alloc_term();
    term->tag = INT;
    term->int_v = value;
    return term;
}

Term_t* new_real(real_t value) {
    Term_t* term = alloc_term();
    term->tag = REAL;
    term->real_v = value;
    return term;
}

Term_t* new_char(char_t value) {
    Term_t* term = alloc_term();
    term->tag = CHAR;
    term->char_v = value;
    return term;
}

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