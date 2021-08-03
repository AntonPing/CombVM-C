#include "Norem.h"

#define STACK_SIZE 64

#define PUSH(reg) \
    sp ++; \
    assert(sp <= stack_ceil); \
    *sp = reg

#define ARG_1 sp[0]
#define ARG_2 sp[-1]
#define ARG_3 sp[-2]
#define ARG_4 sp[-3]

#define POP(n) \
    sp -= n

#define WITH(x) \
    with = x

#define NEXT(clk) \
    step -= clk; \
    continue

#define CALL(x) \
    x = &HOLE; \
    PUSH(with); \
    PUSH(&FRAME); \
    WITH(x); \
    NEXT(2)

#define RESERVE(n) \
    for(int i=0; i<n; i++) \
        assert(sp[-i] != &FRAME)

Term_t FRAME;
Term_t HOLE;

atomic_flag ATOMIC_LOCK = ATOMIC_FLAG_INIT;

#define ATOMIC_LOCK() \
    while(atomic_flag_test_and_set(&ATOMIC_LOCK)){}

#define ATOMIC_UNLOCK() \
    atomic_flag_clear(&ATOMIC_LOCK)

Task_t* new_task(Term_t* with) {
    Task_t* task = malloc(sizeof(Task_t));
    Term_t* *stack = malloc(sizeof(Term_t*) * STACK_SIZE);
    stack[0] = &HOLE;
    stack[1] = &sing[EXIT];
    stack[2] = &FRAME;
    stack[3] = with;
    task->stack_base = stack;
    task->stack_ceil = &stack[STACK_SIZE - 1];
    task->sp = &stack[3];
    return task;
}

void show_task(Task_t* task) {
    Term_t* *stack_base = task->stack_base;
    //Term_t* *stack_ceil = task->stack_ceil;
    Term_t* *sp = task->sp;
    
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



Task_t* eval(Task_t* task, int_t timeslice) {
    //show_task(task);

    // load the task
    Term_t* *stack_base = task->stack_base;
    Term_t* *stack_ceil = task->stack_ceil;
    Term_t* *sp = task->sp;
    Term_t* with = *sp--; // pop top as with
    
    // run timeslice
    assert(timeslice >= 0);
    int_t step = timeslice;
    while(step > 0) {

        #if DEBUG
			PUSH(with);
        	task->stack_base = stack_base;
            task->stack_ceil = stack_ceil;
            task->sp = sp;
			show_task(task);
			with = *sp--;
		#endif

        switch(with->tag) {
            case APP:
                PUSH(with->t2);
                WITH(with->t1);
                NEXT(1);
            case I: 
                RESERVE(1);
                // I x = x
                WITH(ARG_1);
                POP(1);
                NEXT(1);
            case K:
                RESERVE(2);
                // K x y = x
                WITH(ARG_1);
                POP(2);
                NEXT(1);
            case S:
                RESERVE(3);
                // S x y z = (x z) (y z)
                PUSH(new_app(ARG_2,ARG_3));
                PUSH(ARG_3);
                WITH(ARG_1);
                POP(3);
                NEXT(2);

            #define BINOP(t1, t2, expr) \
                RESERVE(2); \
                if(ARG_1->tag == APP) { CALL(ARG_1); } \
                if(ARG_2->tag == APP) { CALL(ARG_2); } \
                assert(ARG_1->tag == t1); \
                assert(ARG_2->tag == t2); \
                WITH(expr); \
                POP(2)
            
            case ADDI:
                BINOP(INT,INT,new_int(ARG_1->int_v + ARG_2->int_v));
                NEXT(2);
            case SUBI:
                BINOP(INT,INT,new_int(ARG_1->int_v - ARG_2->int_v));
                NEXT(2);
            case MULI:
                BINOP(INT,INT,new_int(ARG_1->int_v * ARG_2->int_v));
                NEXT(2);
            case DIVI:
                BINOP(INT,INT,new_int(ARG_1->int_v / ARG_2->int_v));
                NEXT(2);
            case EQL:
                BINOP(INT,INT,new_bool(ARG_1->int_v == ARG_2->int_v));
                NEXT(2);
            case GRT:
                BINOP(INT,INT,new_bool(ARG_1->int_v > ARG_2->int_v));
                NEXT(2);
            case LSS:
                BINOP(INT,INT,new_bool(ARG_1->int_v < ARG_2->int_v));
                NEXT(2);
            
            #undef BINOP

            #define UNIOP(t1, expr) \
                RESERVE(1); \
                if(ARG_1->tag != t1) { CALL(ARG_1); } \
                WITH(expr); \
                POP(1)
            
            case NEGI:
                UNIOP(INT,new_int(ARG_1->int_v * -1));
                NEXT(2);
            case NOT:
                UNIOP(BOOL,new_int(!ARG_1->bool_v));
                NEXT(2);
            case IF: // if p t f
                RESERVE(3);
                if(ARG_1->tag != BOOL) { CALL(ARG_1); }
                WITH(ARG_1->bool_v ? ARG_2 : ARG_3);
                POP(3);
                NEXT(3);
            case PRINTI:
                RESERVE(2);
                if(ARG_1->tag != INT) { CALL(ARG_1); }
                printf("printi: %ld\n",ARG_1->int_v);
                WITH(ARG_2);
                NEXT(5);
            case NEWBOX:
                RESERVE(1);
                WITH(ARG_1);
                POP(1);
                PUSH(new_box());
                NEXT(2);
            case SAVE: { // SAVE box var k => k var
                Term_t *box, *var;
                RESERVE(3);
                box = ARG_1;
                var = ARG_2;
                WITH(ARG_3);
                POP(3);
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
                POP(2);
                ATOMIC_LOCK();
                PUSH(box->box_v);
                ATOMIC_UNLOCK();
                NEXT(3);
            }
            case FORK: // FORK t k
                RESERVE(2);
                send_task(new_task(ARG_1));
                WITH(ARG_2);
                POP(2);
                NEXT(3);
            case EXIT:
                return NULL;
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
                    // cover the hole
                    for(int i=0; ; i++) {
                        if(sp[-i] == &HOLE) {
                            sp[-i] = with;
                        }
                    }
                    WITH(sp[-1]);
                    POP(2);
                    NEXT(2);
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
    return task;
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
    //printf("new task!!!\n");
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
            task = eval(task, 256); // run the timeslice 
            if(task != NULL) {
                send_task(task); // send back the task
            }
        }
    }
}

void task_test() {
    task_module_init();
    Term_t* test = new_app(new_app(&sing[PRINTI],new_int(42)),&sing[EXIT]);
    test = term_compile(test);
    while(true) {
        send_task(new_task(test));
        sleep(1);
    }
    task_module_exit();
}