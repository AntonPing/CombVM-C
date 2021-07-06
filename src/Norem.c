#include "Norem.h"

Term_t tags[256];

void show_term(Term_t* term);
void show_app_list(Term_t* term);

void show_app_list(Term_t* term) {
    if(is_atom(term)) {
        show_term(term);
    } else {
        show_app_list(term->t1);
        printf(" ");
        show_term(term->t2);
    }
}

void show_lamb(Term_t* term);
void show_term(Term_t* term);


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
        printf("<NULL>\n");
        return;
    } else if(is_tag(term)) {
        printf("<tag%ld>",term - &tags[0]);
        return;
    } else if(is_singleton(term)) {
        switch(term - &tags[0]) {
            case S: printf("S"); return;
            case K: printf("K"); return;
            case I: printf("I"); return;
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
        }
    } else if(is_app(term)) {
        printf("(");
        show_app_list(term);
        printf(")");
    } else {
        printf("??");
    }
}


int main() {
    printf("hello\n");
    parser_test();
}