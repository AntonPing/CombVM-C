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

symb_t append_symb(char_t* str) {
    //DBG("new symbol: %s\n",str);
    if(symb_ptr > symb_ceil) {
        PANIC("symbol table overflow!\n");
        return NULL; // symbol table overflow
    } else {
        symb_t symb = malloc(sizeof(char) * (strlen(str) + 1));
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
static Bind_t dictionary[2048];
size_t dict_index = 0;

Bind_t* dict_get_bind(symb_t key) {
    for(size_t i = 0; i < dict_index; i++) {
        if(dictionary[i].name == key) {
            DBG("Found\n");
            return &dictionary[i];
        }
    }
    DBG("Nothing\n");
    return NULL;
}

Bind_t* dict_new_bind(symb_t key) {
    if(dict_get_bind(key) == NULL) {
        Bind_t bind = { key, NULL, true, NULL, NULL, NULL };
        dictionary[dict_index] = bind;
        return &dictionary[dict_index++];
    } else {
        DBG("key already exist.\n");
        return NULL;
    }
}

void bind_define(Bind_t* bind, string_t text) {
    assert(bind != NULL);
    size_t length = strlen(text);
    bind->text = malloc(sizeof(char_t) * (length + 1));
    strcpy(bind->text, text);
    bind->text[length - 1] = '\0';
    bind->raw = term_parse(bind->text);
}

void bind_update(Bind_t* bind) {
    assert(bind != NULL);
    if(bind->updated) {
        return;
    } else {
        bind->linked = term_link(bind->raw);
        bind->compiled = term_compile(bind->linked);
        bind->updated = true;
    }
}
typedef struct Node_t {
    symb_t name;
    bool is_bind;
    union {
        Bind_t* bind;
        struct Node_t* node;
    };
    bool is_header;
    struct Node_t* next;
} Node_t;

static Node_t* wdir = NULL;

Node_t* dict_get(Node_t* from, symb_t key) {
    Node_t* ptr = from;
    while(true) {
        if(ptr == NULL) {
            return NULL;
        } else if(ptr->name == key) {
            return ptr;
        } else {
            ptr = ptr->next;
        }
    }
}

void node_back() {
    wdir = wdir->father;
}

void node_enter(symb_t key) {
    Node_t* node = dict_get(wdir, key);
    if(node == NULL) {
        puts("No such directory!");
    } else {
        wdir = node;
    }
}

void node_pwd() {
    if(wdir == NULL) {
        puts("root.");
    } else {
        node_pwd(wdir->father);
        puts(wdir->name);
    }
}

Node_t* node_new_dir(symb_t key) {
    Node_t* res;
    if(dict_get(wdir,key) != NULL) {
        puts("Directory already exist!");
        return NULL;
    }
    res = malloc(sizeof(Node_t));
    res->name = key;
    res->is_bind = false;
    res->node = NULL;
    res->next = wdir;
    res->father = wdir->father;
    wdir->next = res;
    return res;
}
*/