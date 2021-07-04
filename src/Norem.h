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
typedef char* string_t;

// global tag pointers
extern char tags[256];

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

//Norem.c
void show_term(Term_t* term);

//NoremHeap.c
Term_t* new_app(Term_t* t1, Term_t* t2);
Term_t* new_int(int_t value);
Term_t* new_real(real_t value);
Term_t* new_char(char_t value);
Term_t* new_bool(bool_t value);
Term_t* new_symb(symb_t value);

//NoremParse.c
void parser_test();

//NoremSymb.c
symb_t to_symb(char_t* str);
string_t substr(string_t str, size_t n);

#endif
