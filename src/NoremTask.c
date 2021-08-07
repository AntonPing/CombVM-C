#include "Norem.h"

#define NUM_THREADS 1
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
atomic_flag task_lock = ATOMIC_FLAG_INIT;

#define task_queue_lock() \
    while(atomic_flag_test_and_set(&task_lock)){}

#define task_queue_unlock() \
    atomic_flag_clear(&task_lock)

volatile bool stop_the_world = false;
volatile _Atomic int stopped_num = 0;

extern void run_gc();
void wait_for_gc() {
    if(stop_the_world) {
        stopped_num ++;
        assert(stopped_num <= NUM_THREADS);
        if(stopped_num == NUM_THREADS) {
            //puts("worker running gc");
            run_gc();
            stopped_num = 0;
            stop_the_world = false;
        } else {
            //puts("waiting.........");
            sleep(1);
            while(stop_the_world) {
                sleep(1);
            }
            //puts("back to work!!!!!!!!!!");
        }
    }
}

void* thread_loop();
void task_module_init() {
    task_queue_base = malloc(sizeof(Task_t*) * TASK_QUEUE_LEN);
    task_queue_ceil = &task_queue_base[TASK_QUEUE_LEN - 1];
    task_head = &task_queue_base[0];
    task_tail = &task_queue_base[1];

    for(int i=0; i<NUM_THREADS; i++){
        int ret = pthread_create(&thread_pool[i], NULL, thread_loop, NULL);
        if (ret != 0) {
            PANIC("pthread_create error: %d\n", ret);
        }
    }
}

void task_module_exit() {
    puts("task module exit...");
    for(int i=0; i<NUM_THREADS; i++){
        int ret = pthread_cancel(thread_pool[i]);
        if(ret != 0) {
            PANIC("unable to join worker thread id:%d, errno:%d\n",i,ret);
            exit(-1);
        }
    }
}

Task_t* fetch_task() {
    task_queue_lock();
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
    task_queue_unlock();
    return result;
}

void send_task(Task_t* task){
    task_queue_lock();
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
    task_queue_unlock();
}

void* thread_loop() {
    Task_t* task;
    while(true) {
        wait_for_gc();
        task = fetch_task();
        if(task == NULL) {
            for(int i=0; i<100; i++) {;}; // sleep and wait for new task
        } else {
            if(eval(task, 1024)) { // run the timeslice
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