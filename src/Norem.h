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
typedef bool bool_t;
typedef char* symb_t;

typedef enum Tag_t {
    INT, REAL, CHAR, BOOL, SYMB,
    I,K,S,B,C,SP,BS,CP,Y,
    VAR, ABS, APP, ENV,
    CONS, FUNC, DATA,
    NIL, STR
} Tag_t;

typedef struct Term_t {
    union {
        char* tag;
        struct Term_t* t1;
    };
    union {
        int_t int_v;
        real_t real_v;
        char_t char_v;
        bool_t bool_v;
        symb_t symb_v;
        struct Term_t* t2;
    };
} Term_t;


#endif
