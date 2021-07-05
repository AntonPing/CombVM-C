#include "Norem.h"

static symb_t symb_base[2048];
static symb_t* symb_ceil = &symb_base[2047];
static symb_t* symb_ptr = &symb_base[0];


string_t substr(string_t str, size_t n) {
    char_t* res = malloc(sizeof(char_t) * (n + 2));
    memset(res, '\0', n + 2);
    strncpy(res, str, n);
    return res;
}

symb_t append_symb(char_t* str) {
    if(symb_ptr > symb_ceil) {
        return NULL; // symbol table overflow
    } else {
        symb_t symb = malloc(sizeof(char) * strlen(str));
        strcpy(symb, str);
        *symb_ptr++ = symb;
        return symb;
    }
}

symb_t to_symb(char_t* str) {
    symb_t* ptr;
    for(ptr = symb_base; ptr < symb_ptr; ptr ++) {
        if(strcmp(*ptr,str) == 0) {
            return *ptr;
        }
    }
    return append_symb(str);
}