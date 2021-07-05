#include "Norem.h"

Term_t tags[256];


bool is_tag(Term_t* term) {
    return term >= &tags[0] && term <= &tags[255];
}
bool is_atom(Term_t* term) {
    return is_tag(term) || is_tag(term->tag);
}
bool is_cons(Term_t* term) {
    return !is_tag(term) && !is_tag(term->tag);
}

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
    } else {
        printf("(");
        show_app_list(term);
        printf(")");
    }   
}


int main() {
    printf("hello\n");
    parser_test();
}