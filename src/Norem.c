#include "Norem.h"

Term_t tags[256];

static Dict_t* root = NULL;

Dict_t* dict_get(symb_t key) {
    Dict_t* ptr = root;
    while(ptr != NULL) {
        if(ptr->name == key) {
            return ptr;
        } else {
            ptr = ptr->next;
        }
    }
    return NULL;
}

Dict_t* dict_new(symb_t key) {
    assert(dict_get(key) == NULL);
    Dict_t* res = malloc(sizeof(Dict_t));
    res->name = key;
    res->raw = NULL;
    res->linked = NULL;
    res->compiled = NULL;
    res->text = NULL;
    res->next = root;
    root = res;
    return root;
}

void dict_define(symb_t key, string_t text, Term_t* value) {
    Dict_t* dict = dict_get(key);
    if(dict == NULL) {
        dict = dict_new(key);
        dict->text = text;
        dict->raw = value;
        dict->compiled = term_compile(value);
        puts("Ok.");
    } else {
        puts("Defination already exist!");
    }
}

void dict_update(symb_t key, string_t text, Term_t* value) {
    Dict_t* dict = dict_get(key);
    if(dict != NULL) {
        dict->text = text;
        dict->raw = value;
        dict->compiled = term_compile(value);
        dict->linked = NULL;
        puts("Ok.");
    } else {
        puts("Defination doesn't exist!");
    }
}

void command_define(string_t text, bool update) {
    symb_t key;
    Term_t* value;
    if(definition(text, &key, &value)) {
        if(update) {
            dict_update(key, text, value);
        } else {
            dict_define(key, text, value);
        }
    } else {
        puts("Command Parsing Error!");
    }
}

void command_relink() {
    Dict_t* ptr = root;
    while(ptr != NULL) {
        ptr->linked = NULL;
        ptr = ptr->next;
    }
}



void repl() {
    static char input[2048];

    // Print Version and Exit Information
    puts("Norem Repl Version 0.1");
    puts("Type :quit to Exit\n");

    while(true) {
        fputs("> ", stdout);
        fgets(input, 2048, stdin);
        
        size_t length = strlen(input);
        // delete the last \n in input
        input[length - 1] = '\0';
        length --;

        if(input[0] == ':') {
            if(strcmp(input,":quit") == 0) {
                puts("Goodbye!");
                return;
            } else if(strncmp(input,":dict",5) == 0) {
                show_dict(root);
            } else if(strncmp(input,":define",7) == 0) {
                command_define(&input[7], false);
            } else if(strncmp(input,":update",7) == 0) {
                command_define(&input[7], true);
            } else if(strncmp(input,":relink",7) == 0) {
                command_relink();
            } else {
                puts("Error: Unknown command!");
            }
        } else {
            Term_t* term;
            if(term_parse(input,&term)) {
                command_relink();
                //show_term(term);
                //DBG(" <- after parsing\n");
                term = term_link(term);
                //show_term(term);
                //DBG(" <- after linking\n");
                term = term_compile(term);
                //show_term(term);
                //DBG(" <- after compiling\n");
                term = eval(term);
                //printf("result: ");
                show_term(term);
                printf("\n");
            } else {
                puts("Parser Error!"); 
            }
        }
    }
}


int main() {
    repl();
    return 0;
}
