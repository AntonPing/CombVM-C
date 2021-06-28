#include "Norem.h"

Symb_t* dict_base;
Symb_t* dict_ceil;
Symb_t* dict_ptr;

void dict_init() {
    dict_base = malloc(DICT_SIZE * sizeof(Symb_t));
    printf("dict_init: %o\n", dict_base);
    dict_ceil = dict_base + DICT_SIZE;
    dict_ptr = dict_base;
}


Symb_t* def_basic(char* name, Term_t* (fn)()) {
    Symb_t* symb = to_symb(name);
    symb->state = BASIC;
    symb->fn = fn;
    return symb;
}

Symb_t* def_symb(char* name, Term_t* value) {
    Symb_t* symb = to_symb(name);
    symb->state = LINK;
    symb->value = value;
    return symb;
}

void show_dict() {
    Symb_t* ptr = dict_base;
    printf("---------------------------\n");
    printf("-- DICTIONARY DEFINATION --\n");
    printf("---------------------------\n");
    while(ptr < dict_ptr) {
        printf("%s\t->\t",ptr->key);
        switch(ptr->state) {
            case UNDEF:
                printf("?\n");
                break;
            case BASIC:
                printf("basic\n");
                break;
            case LINK:
                show_term(ptr->value);
                printf("\n");
                break;
        }
        ptr ++;
    }
    printf("---------------------------\n");
}

Symb_t* to_symb(char* str) {
    Symb_t* ptr = dict_base;
    while(ptr < dict_ptr) {
        if(strcmp(ptr->key,str) == 0) {
            return ptr;
        }
        ptr ++;
    }
    assert(ptr == dict_ptr);
    
    char* new = malloc((strlen(str)+1) * sizeof(char));
    strcpy(new,str);
    dict_ptr->key = new;
    dict_ptr->value = NULL;
    return dict_ptr ++;
}
