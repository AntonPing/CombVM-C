#include "Norem.h"
// FLAGS
#define DEBUG_TASK

// TASK
#define STACK_SIZE 32
Term_t FRAME;
Term_t HOLE;

atomic_flag atomic_lock = ATOMIC_FLAG_INIT;

#define ATOMIC_LOCK() \
    while(atomic_flag_test_and_set(&atomic_lock)){}

#define ATOMIC_UNLOCK() \
    atomic_flag_clear(&atomic_lock)

Task_t* new_task(Term_t* with) {
    Task_t* task = malloc(sizeof(Task_t));
    Term_t* *stack = malloc(sizeof(Term_t*) * STACK_SIZE);
    stack[0] = &sing[EXIT];
    stack[1] = &FRAME;
    stack[2] = with;
    task->stack_base = stack;
    task->stack_ceil = &stack[STACK_SIZE - 1];
    task->sp = &stack[2];
    task->ret = NULL;
    return task;
}

void show_task(Task_t* task) {
    Term_t* *stack_base = task->stack_base;
    Term_t* *sp = task->sp;
    printf("# ret = "); show_term(task->ret); printf("\n");
    puts("-------------------------");
    for(Term_t* *ptr = sp; ptr >= stack_base; ptr --) {
        assert(*ptr != NULL);
        if(*ptr == &FRAME) {
            puts("-------------------------");
        } else if(*ptr == &HOLE) {
            puts("| &HOLE");
        } else {
            printf("| "); show_term(*ptr); printf("\n");
        }
    }
    puts("-------------------------");
}

#ifdef DEBUG_TASK
	#define SHOW_TASK() do { \
        PUSH(with); \
        task->stack_base = stack_base; \
        task->stack_ceil = stack_ceil; \
        task->sp = sp; \
        task->ret = ret; \
        show_task(task); \
        with = POP(); \
    } while(0)
#else
    #define SHOW_TASK()
#endif


void stack_extend(Term_t*** base, Term_t*** ceil, Term_t*** sp) {
    size_t new_size = (*ceil - *base + 1) * 2;
    Term_t* *new_base = realloc(*base, new_size * sizeof(Term_t*));
    if(new_base == NULL) {
        PANIC("realloc stack failed! size=%ld\n", new_size);
    }

    *sp = *sp + (new_base - *base);
    *base = new_base;
    *ceil = new_base + new_size - 1;
}

#define ARG_1 sp[0]
#define ARG_2 sp[-1]
#define ARG_3 sp[-2]
#define ARG_4 sp[-3]

#define PUSH(reg) \
    if(sp >= stack_ceil) { \
        stack_extend(&stack_base,&stack_ceil,&sp); \
    } \
    sp ++; \
    *sp = reg

#define POP() \
    *sp--

#define RESERVE(n) do { \
    for(int i=0; i<n; i++) { \
        if(sp[-i] == &FRAME) { RET(); } \
    } \
} while(0)

#define CONSUME(n) \
    sp -= n

#define WITH(x) \
    with = x

#define NEXT(clk) \
    step -= clk; \
    goto eval_loop

#define CALL(x) do { \
    Term_t* caller = with; \
    WITH(x); \
    x = &HOLE; \
    PUSH(caller); \
    PUSH(&FRAME); \
    NEXT(2); \
} while(0)

#define RET() do { \
    assert(ret == NULL); \
    ret = with; \
    with = POP(); \
    while(with != &FRAME) { \
        ret = new_app(ret, with); \
        with = POP(); \
    } \
    with = POP(); \
    NEXT(3); \
} while(0)

#define EVAL(x,check) do { \
    if(x == &HOLE) { \
        x = ret; \
        assert(x != NULL); \
        if(check) { \
            ret = NULL; \
        } else { \
            PANIC("Return with the wrong type!\n"); \
        } \
    } else { \
        if(!(check)) { \
            CALL(x); \
        } \
    } \
} while(0)

bool eval(Task_t* task, int_t timeslice) {
    show_task(task);

    // load the task
    Term_t* *stack_base = task->stack_base;
    Term_t* *stack_ceil = task->stack_ceil;
    Term_t* *sp = task->sp;
    Term_t* ret = task->ret; // ret register
    Term_t* with = *sp--; // with register

    // run timeslice
    assert(timeslice >= 0);
    int_t step = timeslice;

    eval_loop: // while step > 0 run the task
    if(step > 0) {
        assert(with != NULL);

        switch(with->tag) {
            Term_t* temp;
            case APP:
                PUSH(with->t2);
                WITH(with->t1);
                NEXT(1);
            case I:
                // I x = x
                RESERVE(1);
                WITH(ARG_1);
                CONSUME(1);
                NEXT(1);
            case K:
                // K x y = x
                RESERVE(2);
                WITH(ARG_1);
                CONSUME(2);
                NEXT(1);
            case S:
                // S f g x = (f x (g x))
                RESERVE(3);
                temp = ARG_3;
                ARG_3 = new_app(ARG_2,ARG_3),
                ARG_2 = temp;
                WITH(ARG_1);
                CONSUME(1);
                NEXT(2);
            case B:
                // B f g x = (f (g x))
                RESERVE(3);
                ARG_3 = new_app(ARG_2,ARG_3),
                WITH(ARG_1);
                CONSUME(2);
                NEXT(2);
            case C:
                // C f g x = (f x g)
                RESERVE(3);
                temp = ARG_3;
                ARG_3 = ARG_2,
                ARG_2 = temp;
                WITH(ARG_1);
                CONSUME(1);
                NEXT(2);
            case BS:
                // B* c f g x = (c (f (g x)))
                RESERVE(4);
                ARG_4 = new_app(ARG_2,new_app(ARG_3,ARG_4));
                WITH(ARG_1);
                CONSUME(3);
                NEXT(2);
            case CP:
                // C' c f g x = (c (f x) g)
                RESERVE(4);
                temp = ARG_4;
                ARG_4 = ARG_3;
                ARG_3 = new_app(ARG_2,temp);
                WITH(ARG_1);
                CONSUME(2);
                NEXT(2);
            case SP:
                // S' c f g x = (c (f x) (g x))
                RESERVE(4);
                temp = ARG_4;
                ARG_4 = new_app(ARG_3,ARG_4);
                ARG_3 = new_app(ARG_2,temp);
                WITH(ARG_1);
                CONSUME(2);
                NEXT(2);
            case E:
                RESERVE(2);
                // E f x = f x where x is evaluated
                if(ARG_2 == &HOLE) {
                    assert(ret != NULL);
                    ARG_2 = ret;
                    ret = NULL;
                } else {
                    CALL(ARG_2);
                }
                SHOW_TASK();
                WITH(ARG_1);
                CONSUME(1);
                NEXT(2);

            #define BINOP(check1, check2, expr) \
                RESERVE(2); \
                EVAL(ARG_1, check1); \
                EVAL(ARG_2, check2); \
                SHOW_TASK(); \
                WITH(expr); \
                CONSUME(2)
            
            case ADDI:
                RESERVE(2);
                EVAL(ARG_1, ARG_1->tag == INT);
                EVAL(ARG_2, ARG_2->tag == INT);
                SHOW_TASK();
                WITH(new_int(ARG_1->int_v + ARG_2->int_v));
                CONSUME(2);
                NEXT(2);
            case SUBI:
                RESERVE(2);
                EVAL(ARG_1, ARG_1->tag == INT);
                EVAL(ARG_2, ARG_2->tag == INT);
                SHOW_TASK();
                WITH(new_int(ARG_1->int_v - ARG_2->int_v));
                CONSUME(2);
                NEXT(2);
            case MULI:
                RESERVE(2);
                EVAL(ARG_1, ARG_1->tag == INT);
                EVAL(ARG_2, ARG_2->tag == INT);
                SHOW_TASK();
                WITH(new_int(ARG_1->int_v * ARG_2->int_v));
                CONSUME(2);
                NEXT(2);
            case DIVI:
                RESERVE(2);
                EVAL(ARG_1, ARG_1->tag == INT);
                EVAL(ARG_2, ARG_2->tag == INT);
                SHOW_TASK();
                WITH(new_int(ARG_1->int_v / ARG_2->int_v));
                CONSUME(2);
                NEXT(2);
            case EQL:
                RESERVE(2);
                EVAL(ARG_1, ARG_1->tag == INT);
                EVAL(ARG_2, ARG_2->tag == INT);
                SHOW_TASK();
                WITH(new_bool(ARG_1->int_v == ARG_2->int_v));
                CONSUME(2);
                NEXT(2);
            case GRT:
                RESERVE(2);
                EVAL(ARG_1, ARG_1->tag == INT);
                EVAL(ARG_2, ARG_2->tag == INT);
                SHOW_TASK();
                WITH(new_bool(ARG_1->int_v > ARG_2->int_v));
                CONSUME(2);
                NEXT(2);
            case LSS:
                RESERVE(2);
                EVAL(ARG_1, ARG_1->tag == INT);
                EVAL(ARG_2, ARG_2->tag == INT);
                SHOW_TASK();
                WITH(new_int(ARG_1->int_v < ARG_2->int_v));
                CONSUME(2);
                NEXT(2);
            
            #undef BINOP

            #define UNIOP(check1, expr) \
                RESERVE(1); \
                EVAL(ARG_1, check1); \
                SHOW_TASK(); \
                WITH(expr); \
                CONSUME(1)
            
            case NEGI:
                RESERVE(1);
                EVAL(ARG_1, ARG_1->tag == INT);
                SHOW_TASK();
                WITH(new_int(ARG_1->int_v * -1));
                CONSUME(1);
                NEXT(2);
            case NOT:
                RESERVE(1);
                EVAL(ARG_1, ARG_1->tag == BOOL);
                SHOW_TASK();
                WITH(new_bool(!ARG_1->bool_v));
                CONSUME(1);
                NEXT(2);
            
            #undef BINOP

            case IF:
                RESERVE(3);
                EVAL(ARG_1, ARG_1->tag == BOOL);
                WITH(ARG_1->bool_v ? ARG_2 : ARG_3);
                CONSUME(3);
                NEXT(3);
            case PRINTI:
                RESERVE(2);
                EVAL(ARG_1, ARG_1->tag == INT);
                printf("printi: %ld\n",ARG_1->int_v);
                WITH(ARG_2);
                CONSUME(2);
                NEXT(5);
            /*
            case NEWBOX:
                RESERVE(1);
                WITH(ARG_1);
                CONSUME(1);
                PUSH(new_box());
                NEXT(2);
            case SAVE: { // SAVE box var k => k var
                Term_t *box, *var;
                RESERVE(3);
                box = ARG_1;
                var = ARG_2;
                WITH(ARG_3);
                CONSUME(3);
                ATOMIC_LOCK();
                box->box_v = var;
                ATOMIC_UNLOCK();
                NEXT(3);
            }
            case LOAD: { // LOAD box k => k result
                Term_t *box;
                RESERVE(2);
                box = ARG_1;
                WITH(ARG_2);
                CONSUME(2);
                ATOMIC_LOCK();
                PUSH(box->box_v);
                ATOMIC_UNLOCK();
                NEXT(3);
            }
            case FORK: // FORK t k
                RESERVE(2);
                send_task(new_task(ARG_1));
                WITH(ARG_2);
                CONSUME(2);
                NEXT(3);
            */
            case EXIT:
                task->stack_base = stack_base;
                task->stack_ceil = stack_ceil;
                task->sp = sp;
                task->ret = ret;
                return false;
            case SYMB: {
                Dict_t* dict = dict_get(with->symb_v);
                if(dict != NULL) {
                    assert(dict->compiled != NULL);
                    LOG("dynamic linking %s ...\n",with->symb_v);
                    WITH(dict->compiled);
                    NEXT(3);
                } else {
                    PANIC("undefined symbol!\n");
                }
            }
            case INT:
            case REAL:
            case CHAR:
            case BOOL:
                // BASIC DATA
                if(sp[0] == &FRAME) {
                    RET();
                } else {
                    printf("DATA1: ");
                    show_term(with);
                    printf("\nDATA2:");
                    show_term(sp[0]);
                    PANIC("\nCannot apply data to data!\n");
                }
            default:
                PANIC("Unknown tag while evaling term! %d\n", with->tag);
        }
    }

    PUSH(with);
    task->stack_base = stack_base;
    task->stack_ceil = stack_ceil;
    task->sp = sp;
    task->ret = ret;
    return true;
}

/////////////////////////
//    MULTITASKING     //
/////////////////////////

#define NUM_THREADS 8
#define TASK_QUEUE_LEN 256
pthread_t thread_pool[NUM_THREADS];
Task_t* *task_queue_base;
Task_t* *task_queue_ceil;
Task_t* *task_head;
Task_t* *task_tail;
// task_head + 1 == task_tail
// when and only when there is no task
// task_head == task_tail
// when and only when task full 

atomic_flag task_pool_lock = ATOMIC_FLAG_INIT;

#define task_pool_lock() \
    while(atomic_flag_test_and_set(&task_pool_lock)){}

#define task_pool_unlock() \
    atomic_flag_clear(&task_pool_lock)

void* thread_loop();
void task_module_init() {
    task_queue_base = malloc(sizeof(Task_t*) * TASK_QUEUE_LEN);
    task_queue_ceil = &task_queue_base[TASK_QUEUE_LEN - 1];
    task_head = &task_queue_base[0];
    task_tail = &task_queue_base[1];
    for(int i=0; i<NUM_THREADS; i++){
        //参数依次是：创建的线程id，线程参数，调用的函数，传入的函数参数
        int ret = pthread_create(&thread_pool[i], NULL, thread_loop, NULL);
        if (ret != 0) {
            PANIC("pthread_create error: %d\n", ret);
        }
    }
}

void task_module_exit() {
    for(int i=0; i<NUM_THREADS; i++){
        pthread_cancel(thread_pool[i]);
    }
}

Task_t* fetch_task() {
    task_pool_lock();
    Task_t* result;
    if(task_head + 1 == task_tail) {
        result = NULL;
    } else if(task_head == task_queue_ceil
            && task_tail == task_queue_base) {
        result = NULL;
    } else {
        if(task_head == task_queue_ceil) {
            task_head = task_queue_base;
        } else {
            task_head ++;
        }
        result = *task_head;
    }
    task_pool_unlock();
    return result;
}

void send_task(Task_t* task){
    task_pool_lock();
    if(task_head == task_tail) {
        PANIC("task overflow! MAX=%d\n",TASK_QUEUE_LEN);
    } else {
        *task_tail = task;
        if(task_tail == task_queue_ceil) {
            task_tail = task_queue_base;
        } else {
            task_tail ++;
        }
    }
    task_pool_unlock();
}

void* thread_loop(){
    while(true) {
        Task_t* task = fetch_task();
        if(task == NULL) {
            sleep(1); // sleep and wait for new task
        } else {
            if(eval(task, 256)) { // run the timeslice
                // send back the task if it was interrupted
                send_task(task); 
            } else {
                // delete the task if it was finished
                printf("task completed! ret = ");
                show_term(task->ret);
                printf("\n");
                free(task);
                //LOG("task completed!");
            }
        }
    }
}