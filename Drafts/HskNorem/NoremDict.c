#include "Norem.h"

Term_t* link_raw(Term_t* raw);
Term_t* link_key(Key_t* key);
Key_t* get_key(char* name);

Key_t* dict;
int len;

Key_t* get_key(char* name) {
    for(int i=0; i<len; i++) {
        if(strcmp(dict[i].name, name) == 0) {
            return &dict[i];
        }
    }
    return NULL;
}

char* get_name(Term_t* (*func) (int)) {
    for(int i=0; i<len; i++) {
        if(dict[i].linked->as_func == func) {
            return dict[i].name;
        } else {
            return NULL;
        }
    }
}

void new_raw(char* name, Term_t* raw) {
    dict[len].name = name;
    dict[len].updated = false;
    dict[len].raw = raw;
    dict[len].linked = NULL;
    len ++;
    if(len >= DICT_SIZE) {
        perror("To much defination!");
    }
}

void new_predef(char* name, Term_t* (*func) (int)) {
    dict[len].name = name;
    dict[len].updated = true;
    dict[len].raw = NULL;
    dict[len].linked = new_func(func);
    len ++;
    if(len >= DICT_SIZE) {
        perror("To much defination!");
    }
}

Term_t* link_key(Key_t* key) {
    if(key->updated) {
        return key->linked;
    } else if(key->raw == NULL) {
        // it's predefined;
        return key->linked;
    } else {
        key->linked = link_raw(key->raw);
        key->updated = true;
        return key->linked;
    }
}

Term_t* link_raw(Term_t* raw) {
    if(is_app(raw)) {
        return new_app(link_raw(raw->fun),link_raw(raw->arg));
    } else {
        switch(raw->tag - tag_base) {
            case tag_link:
                if(get_key(raw->as_link) == NULL) {
                    printf("Link failed: Can't find the key '%s' in dict\n",
                        raw->as_link);
                    return NULL;
                }
                return link_key(get_key(raw->as_link));
            case tag_func:
            case tag_int:
            case tag_real:
                return raw;
        }
    }
}

void show_rec(Term_t* term) {
    if(is_app(term)) {
        printf("(");
        show_rec(term->fun);
        show_rec(term->arg);
        printf(")");
    } else {
        switch(term->tag - tag_base) {
            case tag_link: printf("@%s ",term->as_link); break;
            case tag_func: printf("%s ",get_name(term->as_func)); break;
            case tag_int: printf("int %ld ",term->as_int); break;
            case tag_real: printf("real %lf ",term->as_real); break;
        }
    }
}

void show_stack() {
    Term_t** ptr = stack;
    printf("stack: [");
    while(ptr < stack_ceil) {
        ptr++;
        show_rec(*ptr);
        printf(", ");
    }
    printf("]\n");
}

void show_term(Term_t* term) {
    printf("with: ");
    show_rec(term);
    printf("\n");
}

Term_t* s_comb(int args) {
    printf("S... ");
    if(args < 3) {
        return NULL;
    }
    Term_t* f = stack[1];
    Term_t* g = stack[2];
    Term_t* x = stack[3];
    stack += 3;
    push(new_app(g,x));
    push(x);
    printf(",ok!\n");
    return f;//new_app(f,x),new_app(g,x);
}

Term_t* i_comb(int args) {
    printf("I... ");
    if(args < 1) {
        return NULL;
    }
    Term_t* x = stack[1];
    stack += 1;
    printf(",ok!\n");
    return x;
}

Term_t* func_add(int args) {
    printf("ADD... ");
    if(args < 2) {
        return NULL;
    }
    Term_t* a = eval(stack[1]);
    Term_t* b = eval(stack[2]);
    stack += 2;
    printf(",ok!\n");
    return new_int(a->as_int + b->as_int);
}

Term_t* func_if(int args) {
    if(args < 3) {
        return NULL;
    }
    Term_t* cond = eval(stack[1]);
    Term_t* if_true = stack[2];
    Term_t* if_false = stack[3];
    stack += 3;
    if(cond->as_int == 0) {
        return if_false;
    } else {
        return if_true;
    }
}

Term_t* func_show_int(int args) {
    if(args < 1) {
        return NULL;
    }
    Term_t* result = eval(stack[1]);
    stack += 1;
    printf("show_int: %ld\n", result->as_int);
    return eval(result);
}

void dict_init() {
    dict = malloc(DICT_SIZE * sizeof(Key_t));
    len = 0;
    new_predef("S",s_comb);
    new_predef("I",i_comb);
    new_predef("+",func_add);
    new_predef("Show",func_show_int);
    new_raw("Double", new_app(new_app(
            new_link("S"),
            new_link("+")),
            new_link("I")));

    new_raw("Test", new_app(
            new_link("Show"),
            new_app(
                new_link("Double"),
                new_int(5))));
    printf("Init finish: total %d\n", len);
    update_all();

}

Term_t* eval_name(char* name) {
    if(get_key(name) != NULL) {
        return eval(get_key(name)->linked);
    } else {
        printf("Eval failed : no such a def\n");
        return NULL;
    }
}

void update_all() {
    for(int i=0; i<len; i++) {
        dict[i].updated = false;
    }
    for(int i=0; i<len; i++) {
        link_key(&dict[i]);
    }
}