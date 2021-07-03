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
#include <ctype.h>

typedef uint64_t int_t;
typedef double real_t;
typedef char char_t;
typedef char* Symb_t;

typedef enum Tag_t {
    VAR, ABS, APP, ENV,
    DEBUG,
    CONS, FUNC, DATA,
    INT, REAL, CHAR, SYMB,
    BOOL, NIL, STR
} Tag_t;

typedef struct Term_t {
    Tag_t tag;
    union {
        struct { // Abs,Env,Clos
            struct Symb_t* x;
            struct Term_t* t;
        };
        struct { // App & Cons
            struct Term_t* t1;
            struct Term_t* t2;
        };
        struct { // STR
            char* as_str;
            size_t str_len;
        };
        struct Symb_t* as_symb;
        struct Term_t* (*as_func)();
        struct Term_t* as_term;
        int64_t as_int;
        double as_real;
        char as_char;
        bool as_bool;
    };
} Term_t;

typedef struct Syntax_t {
    Tag_t tag;
    union {
        struct { // Abs,Env,Clos
            struct Symb_t* x;
            struct Term_t* t;
        };
        struct { // App & Cons
            struct Term_t* t1;
            struct Term_t* t2;
        };
        struct { // STR
            char* as_str;
            size_t str_len;
        };
        struct Symb_t* as_symb;
        struct Term_t* (*as_func)();
        struct Term_t* as_term;
        int64_t as_int;
        double as_real;
        char as_char;
        bool as_bool;
    };
} Term_t;

typedef enum state_t {
    UNDEF, LINK, BASIC,
} state_t;

typedef struct Symb_t {
    char* key;
    state_t state;
    union {
        Term_t* value;
        Term_t* (*fn)();
    };
} Symb_t;

#define STACK_SIZE 1024
#define HEAP_SIZE 20000
#define SYMB_POOL_SIZE 1024

#define TRASH_SIZE 1024
#define DICT_SIZE 1024

#endif
