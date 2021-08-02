#include "Norem.h"

void show_term(Term_t* term);
void show_lamb(Term_t* term);
void show_app_list(Term_t* term);

void show_lamb(Term_t* term) {
    assert(is_lamb(term));
    printf("(λ %s",term->x);

    Term_t* with = term->t;
    while(true) {
        if(is_lamb(with)) {
            printf(" %s", with->x);
            with = with->t;
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
    }

    switch(term->tag) {
        case S: printf("S"); return;
        case K: printf("K"); return;
        case I: printf("I"); return;
        case ADDI: printf("+"); return;
        case SUBI: printf("-"); return;
        case MULI: printf("*"); return;
        case DIVI: printf("/"); return;
        case NEGI: printf("~"); return;
        case IF: printf("if"); return;
        case NOT: printf("not"); return;
        case EQL: printf("="); return;
        case GRT: printf(">"); return;
        case LSS: printf("<"); return;
        case PRINTI: printf("print"); return;
        case EXIT: printf("EXIT"); return;
        case NIL: printf("nil"); return;
        case INT:
            printf("%ld",term->int_v); return;
        case REAL:
            printf("%lf",term->real_v); return;
        case CHAR:
            printf("%c",term->char_v); return;
        case BOOL:
            printf(term->bool_v ? "true" : "false"); return;
        case SYMB:
            printf("%s",term->symb_v); return;
        case LAMB:
            show_lamb(term); return;
        case APP:
            printf("(");
            show_app_list(term);
            printf(")");
            return ;
        default:
            printf("??"); return;
    }
}

void show_app_list(Term_t* term) {
    assert(term != NULL);
    if(is_app(term)) {
        show_app_list(term->t1);
        printf(" ");
        show_term(term->t2);
    } else {
        show_term(term);
    }
}



void show_dict(Dict_t* dict) {
    puts("-------------------------");
    puts("| DICTIONARY DEFINITION |");
    puts("-------------------------");
    Dict_t* ptr = dict;
    while(ptr != NULL) {
        printf("%s = ",ptr->name);
        show_term(ptr->raw);
        printf("\n");
        ptr = ptr->next;
    }
    puts("-------------------------");
}