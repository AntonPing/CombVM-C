#ifndef NOREM_H
#define NOREM_H
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <stdatomic.h>
#include <unistd.h>
#include <pthread.h>
#include <setjmp.h>
#include <assert.h>
#include <ctype.h>
#include <uchar.h>

typedef int rc_t;
typedef long int_t;
typedef double real_t;
typedef char char_t;
typedef bool bool_t;
typedef char* symb_t;
typedef char* string_t;

#ifdef DEBUG
#define LOG(...) do{ \
    fprintf(stderr, "[LOG]%s %s(Line %d): ",__FILE__,__FUNCTION__,__LINE__); \
    fprintf(stderr, __VA_ARGS__); \
    } while(0)
#else
#define LOG(...)
#endif


#define PANIC(...) do{ \
    fprintf(stderr, "[PANIC]%s %s(Line %d): ",__FILE__,__FUNCTION__,__LINE__); \
    fprintf(stderr, __VA_ARGS__); \
    exit(1); \
} while(0)


typedef enum tag_t {
    INT=0, REAL, CHAR, BOOL, SYMB,
    APP, LAMB, FUNC, CONS, TERM, STR,

    I=100,K,S,B,C,SP,BS,CP,Y,
    ADDI,SUBI,MULI,DIVI,NEGI,
    IF,NOT,EQL,GRT,LSS,
    //ALLOC,FREE,
    //READI,WRITEI
    PRINTI,EXIT,
    NIL,
} tag_t;

typedef struct Term_t {
    tag_t tag;
    rc_t rc;
    union {
        int_t int_v;
        real_t real_v;
        char_t char_v;
        bool_t bool_v;
        symb_t symb_v;
        struct { // App, Cons
            struct Term_t* t1;
            struct Term_t* t2;
        };
        struct { // Lamb
            symb_t x;
            struct Term_t* t;
        };
    };
} Term_t;

/*
typedef struct Type_t {
    string_t name;
    string_t (*format)(Term_t*);
    void (*refer)();
    void (*defer)();
} Type_t;
*/

typedef struct Dict_t {
    symb_t name;
    Term_t* raw;
    Term_t* compiled;
    Term_t* linked;
    string_t text;
    struct Dict_t* next;
} Dict_t;

// Norem.c
extern Term_t sing[256];
Dict_t* dict_get(symb_t key);

// NoremShow.c
void show_term(Term_t* term);
void show_lamb(Term_t* term);
void show_dict(Dict_t* dict);

// NoremCompile.c
bool is_app(Term_t* term);
bool is_lamb(Term_t* term);
bool is_var(Term_t* term);
Term_t* term_compile(Term_t* term);
Term_t* term_link(Term_t* term);

// NoremHeap.c
Term_t* gc_refer(Term_t* term);
void gc_deref(Term_t* term);
void free_term(Term_t* term);

Term_t* raw_app(Term_t* t1, Term_t* t2);
Term_t* new_app(Term_t* t1, Term_t* t2);
Term_t* new_cons(Term_t* t1, Term_t* t2);
Term_t* new_int(int_t value);
Term_t* new_real(real_t value);
Term_t* new_char(char_t value);
Term_t* new_bool(bool_t value);
Term_t* new_symb(symb_t value);
Term_t* new_lamb(symb_t x, Term_t* t);

// NoremParse.c
bool is_space(char_t c);
bool term_parse(char_t* str, Term_t** ret);
bool definition(char_t* str, symb_t* key, Term_t** value);

// NoremEval.c
Term_t* eval(Term_t* term);

// NoremSymb.c
symb_t to_symb(char_t* str);
string_t substr(string_t str, size_t n);
string_t slice(char_t* start, char_t* end);

#endif
