#include "Norem.h"

Term_t tags[256];

void show_term(Term_t* term);
void show_lamb(Term_t* term);
void show_app_list(Term_t* term);

void show_lamb(Term_t* term) {
    assert(term->tag == &tags[LAMB]);
    printf("(λ %s",term->lamb_v->x);

    Term_t* with = term->lamb_v->t;
    while(true) {
        if(with->tag == &tags[LAMB]) {
            printf(" %s", with->lamb_v->x);
            with = with->lamb_v->t;
        } else {
            printf(" -> ");
            show_app_list(with);
            printf(")");
            return;
        }
    }
}

void show_term(Term_t* term) {
    if(term == NULL) {
        printf("<NULL>");
        return;
    } else if(is_tag(term)) {
        printf("<tag%ld>",term - &tags[0]);
        return;
    } else if(is_singleton(term)) {
        switch(term - &tags[0]) {
            case S: printf("S"); return;
            case K: printf("K"); return;
            case I: printf("I"); return;
            case ADDI: printf("+"); return;
            case PRINTI: printf("print"); return;
            case EXIT: printf("EXIT"); return;
            case NIL: printf("Nil"); return;
            default: printf("<Op?>"); return;
                //PANIC("unknown operator %ld",term - &tags[0]);
        }
    } else if(is_tag(term->tag)) {
        switch(term->tag - &tags[0]) {
            case INT:
                printf("%ld",term->int_v);
                return;
            case REAL:
                printf("%lf",term->real_v);
                return;
            case CHAR:
                printf("%c",term->char_v);
                return;
            case BOOL:
                printf(term->bool_v ? "True" : "False");
                return;
            case SYMB:
                printf(term->symb_v);
                return;
            case LAMB:
                show_lamb(term);
                return;
            case NIL:
                printf("Nil");
                return;
            default:
                PANIC("unknown tag");
        }
    } else if(is_app(term)) {
        printf("(");
        show_app_list(term);
        printf(")");
        return ;
    } else {
        printf("??");
    }
}

void show_app_list(Term_t* term) {
    if(term == NULL) {
        show_term(NULL);
    } else if(is_app(term)) {
        show_app_list(term->t1);
        printf(" ");
        show_term(term->t2);
    } else {
        show_term(term);
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
        // delete the last \n in input
        input[strlen(input) - 1] = '\0';
        if(input[0] == ':') {
            if(strcmp(input,":quit") == 0) {
                puts("Goodbye!");
                return;
            } else if(strcmp(input,":dict") == 0) {
                puts("dict!\n");
            } else if(strncmp(input,":define",7) == 0) {
                symb_t key;
                Term_t* value;
                if(definition(&input[7],&key,&value)) {
                    dict_new_key(key,value);
                    puts("Ok.\n");
                } else {
                    puts("Definition Error!\n");
                }
            } else {
                puts("Error: Unknown command!\n");
            }
        } else {
            Term_t* term;
            if(parse(input,&term)) {
                show_term(term);
                printf(" <- after parsing\n");
                term = linking(term);
                show_term(term);
                printf(" <- after linking\n");
                term = ski_compile(term);
                show_term(term);
                printf(" <- after compiling\n");
                term = eval(term);
                printf("result: ");
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
