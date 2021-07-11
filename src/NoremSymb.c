#include "Norem.h"

static symb_t symb_base[2048];
static symb_t* symb_ceil = &symb_base[2047];
static symb_t* symb_ptr = &symb_base[0];


string_t substr(string_t str, size_t len) {
    char_t* res = malloc(sizeof(char_t) * (len + 1));
    memset(res, '\0', len + 1);
    strncpy(res, str, len);
    return res;
}

string_t slice(char_t* start, char_t* end) {
    size_t len = end - start + 1;
    char_t* res = malloc(sizeof(char_t) * (len + 1));
    memset(res, '\0', len + 1);
    strncpy(res, start, len);
    return res;
}

//string_t concat()
symb_t append_symb(char_t* str) {
    //DBG("new symbol: %s\n",str);
    if(symb_ptr > symb_ceil) {
        PANIC("symbol table overflow!\n");
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
            //DBG("found symbol: %s\n",str);
            return *ptr;
        }
    }
    return append_symb(str);
}



/*
typedef struct Module_t {
    symb_t name;
    size_t cap;
    size_t size;
    union {
        Term_t* term;
        Module_t* list;
    }
}
*/

static symb_t dict_key[2048];
static Term_t* dict_value[2048];
static size_t dict_index = 0;

Term_t* dict_get_value(symb_t key) {
    for(size_t i = 0; i < dict_index; i++) {
        if(dict_key[i] == key) {
            DBG("Found\n");
            return dict_value[i];
        }
    }
    DBG("Nothing\n");
    return NULL;
}

bool dict_update(symb_t key, Term_t* value) {
    for(size_t i = 0; i < dict_index; i++) {
        if(dict_key[i] == key) {
            dict_value[i] = value;
            return false;
        }
    }
    dict_key[dict_index] = key;
    dict_value[dict_index] = value;
    dict_index ++;
    return true;
}

bool dict_new_key(symb_t key, Term_t* value) {
    for(size_t i = 0; i < dict_index; i++) {
        if(dict_key[i] == key) {
            DBG("key already exist!\n");
            return false;
        }
    }
    dict_key[dict_index] = key;
    dict_value[dict_index] = value;
    dict_index ++;
    return true;
}
