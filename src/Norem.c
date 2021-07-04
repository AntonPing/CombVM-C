#include "Norem.h"

char tags[256];

bool is_atom(Term_t* term) {
    return term->tag >= &tags[0] && term->tag <= &tags[255];
}

bool is_cons(Term_t* term) {
    return term->tag < &tags[0] || term->tag > &tags[255];
}

void show_term(Term_t* term);
void show_app_list(Term_t* term);

void show_app_list(Term_t* term) {
    if(is_atom(term)) {
        printf("(");
        show_term(term);
    } else {
        show_app_list(term->t1);
        printf(" ");
        show_term(term->t2);
    }
}

void show_term(Term_t* term) {
    if(term == NULL) {
        printf("NULL\n");
        return;
    }
    printf("here17\n");
    if(is_atom(term)) {
        printf("here18\n");
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
            case NIL:
                printf("Nil");
                return;
        }
    } else {
        show_app_list(term);
        printf(")");
    }   
}


int main() {
    printf("hello\n");
    parser_test();
}