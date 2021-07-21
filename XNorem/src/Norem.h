#ifndef NOREM_H
#define NOREM_H


typedef long int_t;
typedef double real_t;
typedef char char_t;
typedef bool bool_t;
typedef char* symb_t;
typedef char* string_t;

#ifdef DEBUG
#define DBG(...) do{ \
    fprintf(stderr, "[DEBUG]%s %s(Line %d): ",__FILE__,__FUNCTION__,__LINE__); \
    fprintf(stderr, __VA_ARGS__); \
    } while(0)
#else
#define DBG(...)
#endif

#define PANIC(...) do{ \
    fprintf(stderr, "[PANIC]%s %s(Line %d): ",__FILE__,__FUNCTION__,__LINE__); \
    fprintf(stderr, __VA_ARGS__); \
    exit(1); \
} while(0)


typedef enum Tag_t {
    INT=0, REAL, CHAR, BOOL, SYMB,
    LAMB, FUNC, CONS, TERM, STR,

    I=100,K,S,B,C,SP,BS,CP,Y,
    ADDI,SUBI,MULI,DIVI,NEGI,
    IF,NOT,EQL,GRT,LSS,
    //ALLOC,FREE,
    //READI,WRITEI
    PRINTI,EXIT,
    NIL,
} Tag_t;

struct Term_t;

typedef struct Lambda_t {
    symb_t x;
    struct Term_t* t;
} Lambda_t;

typedef struct Term_t {
    union {
        struct Term_t* tag;
        struct Term_t* t1;
    };
    union {
        int_t int_v;
        real_t real_v;
        char_t char_v;
        bool_t bool_v;
        symb_t symb_v;
        struct Lambda_t* lamb_v;
        struct Term_t* t2;
    };
} Term_t;

#endif
