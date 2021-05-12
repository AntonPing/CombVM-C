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

typedef uint64_t Int_t;
typedef double Real_t;
typedef uint32_t Char_t;

struct Symb_t;

typedef enum Tag_t {
    VAR=0, ABS=1, APP=2, ENV=3,
    CONS=4, FUNC=5, DATA=6,
    INT, REAL, CHAR, SYMB,
    NIL, STR
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
#define HEAP_SIZE 1024
#define SYMB_POOL_SIZE 1024

#define TRASH_SIZE 1024
#define DICT_SIZE 1024

// NoremMem.c
void error(char* msg);
Term_t* stack_rewind(Term_t* arg_term);
void push(Term_t* arg_term);
Term_t* pop();

void heap_init();
void stack_init();
Term_t* _Var(Symb_t* x);
Term_t* _Var2(char* x);
Term_t* _Abs(Symb_t* x, Term_t* t);
Term_t* _Abs2(char* x, Term_t* t);
Term_t* _App(Term_t* t1, Term_t* t2);
Term_t* _Env(Symb_t* x, Term_t* t);
Term_t* _Int(int64_t n);
Term_t* _Real(double x);
Term_t* _Func(Term_t* (*fn)());
Term_t* _Symb(Symb_t* str);
Term_t* _Cons(Term_t* h, Term_t* t);
Term_t* _Nil();

// NoremEval.c
Term_t* eval(Term_t* term);
Term_t* apply_env(Term_t* env, Term_t* arg_term);

// NoremTest.c
void show_term(Term_t* arg_term);
Term_t* purify(Term_t* arg_term);
bool is_list(Term_t* arg_term);

// NoremDict.c
void dict_init();
void show_dict();
Symb_t* def_basic(char* name, Term_t* (*fn)());
Symb_t* def_symb(char* name, Term_t* value);
Symb_t* to_symb(char* str);

// NoremFunc.c
Term_t* iADD();
Term_t* iPRINT();
Term_t* EXIT();
Term_t* PAIR();
Term_t* HEAD();
Term_t* TAIL();

// NoremParse.c
Term_t* compile(char* text);
Term_t* reverse(Term_t* list);
Term_t* tokenize(char* text);
Term_t* parse(Term_t* term);

#endif
