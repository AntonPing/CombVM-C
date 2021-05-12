#include "Norem.h"

bool is_list(Term_t* arg_term) {
    Term_t* term = arg_term;

    while(true) {
        switch(term->tag) {
            case CONS:
                term = term->t2;
                continue;
            case NIL:
                return true;
            default:
                return false;
        }
    }
}

void show_list(Term_t* list) {
    Term_t* term = list;
    bool first_time;

    switch(term->tag) {
        case CONS:
            first_time = true;
            printf("[");
            while(true) {
                switch(term->tag) {
                    case CONS:
                        if(first_time) {
                            first_time = false;
                        } else {
                            printf(",");
                        }
                        show_term(term->t1);
                        term = term->t2;
                        continue;
                    case NIL:
                        printf("]");
                        return;
                    default:
                        error("show_list takes a list!\n");
                }
            }
        case NIL:
            printf("NIL");
            return;
        default:
            show_term(term);
    }    
}

Term_t* purify(Term_t* term) {
    switch(term->tag) {
        case ABS:
            return _Abs(term->x,purify(term->t));
        case APP:
            if(term->t1->tag == ENV) {
                return purify(apply_env(term->t1,term->t2));
            } else {
                return _App(purify(term->t1),purify(term->t2));
            }
        default:
            return term;
    }
}

void show_term(Term_t* arg_term) {
    Term_t* term = arg_term;

    //printf("show ");
    //printf("tag=%d,ptr=%o,org=%o\n",term->tag,term,arg_term);
    switch(term->tag) {
        case VAR:
            printf(term->x->key);
            return;
        case ABS:
            printf("λ%s.",term->x->key);
            show_term(term->t);
            return;
        case APP:
            printf("(");
            show_term(term->t1);
            printf(" ");
            show_term(term->t2);
            printf(")");
            return;
        case ENV:
            printf("%s->",term->x->key);
            show_term(term->t);
            return;
        case CONS:
            if(is_list(term)) {
                show_list(term);
            } else {
                printf("(");
                show_term(term->t1);
                printf(",");
                show_term(term->t2);
                printf(")");
            }
            return;
        case INT:
            printf("%ld",term->as_int);
            return;
        case REAL:
            printf("%lf",term->as_real);
            return;
        case CHAR:
            printf("%c",term->as_char);
            return;
        case NIL:
            printf("NIL");
            return;
        case FUNC:
            printf("func");
            return;
        case SYMB:
            printf(term->as_symb->key);
    }
}

void test() {
    /*
    def_basic("iADD",iADD);
    def_basic("iPRINT",iPRINT);
    def_basic("EXIT",EXIT);
    
    def_symb("test1",compile(
        "swap a b"));
    */
    def_symb("swap",compile(
        "\\x. www \\y. y x"));
    def_symb("test2",compile(
        "iADD 1 2 ; iPRINT EXIT"));

    show_dict();
    Term_t* result = eval(_Var2("test2"));
    printf("after:\n");
    show_term(result);
    printf("\n");
    return;
}

void test_parse() {
    Term_t* term;
    term = tokenize("\\x. \\y. ys d \\ss. ss x");
    printf("ok\n");
    show_term(term);
    printf("\n");
    term = parse(term);
    show_term(term);
}

int main() {
    heap_init();
    stack_init();
    dict_init();
    //show_term(compile("iADD 1 2 (iPRINT EXIT)"));
    test();
    //test_parse();

    return 0;
}