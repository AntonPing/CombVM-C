#ifndef NOREM_H
#define NOREM_H
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdatomic.h>
#include <unistd.h>
#include <pthread.h>
#include <stdbool.h>
#include <setjmp.h>
#include <assert.h>


#define TAG_SIZE 256
#define tag_ceil tag_base + TAG_SIZE
#define is_tag(ptr) ptr >= tag_base && ptr <= tag_ceil
#define is_not_tag(ptr) ptr < tag_base || ptr > tag_ceil
#define tag_end TAG_SIZE

extern char tag_base[256];

typedef data_t uint64_t


typedef enum Tag_t {
    INT,REAL,
    S,K,I,
};

typedef union Term_t {
    struct {
        union Term_t* fun;
        union Term_t* arg;
    };
    struct {
        char* tag;
        data_t data;
    };
} Term_t;

typedef struct Key_t {
    char* name;
    char* text;
    Term_t* compiled;
    Term_t* optimized;
} Key_t;



#define STACK_SIZE 1024
#define HEAP_SIZE 1024
#define TRASH_SIZE 1024
#define DICT_SIZE 1024

extern Term_t** stack;
extern Term_t** stack_base;
extern Term_t** stack_ceil;

extern Term_t* heap;
extern Term_t* heap_base;
extern Term_t* heap_ceil;

extern Term_t** trash;
extern Term_t** trash_base;
extern Term_t** trash_ceil;

Term_t* eval(Term_t* term);

void mem_init();
void push(Term_t* term);
Term_t* pop();
Term_t* new_term();
Term_t* new_app(Term_t* fun, Term_t* arg);
Term_t* new_int(int64_t value);
Term_t* new_func(void* func);
Term_t* new_link(char* name);

void dict_init();

Term_t* eval_name(char* name);
void update_all();

#endif
