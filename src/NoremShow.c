#include "Norem.h"

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


/*
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
*/