#include "Norem.h"

Term_t* compile(char* text) {
    Term_t* list_token;

    list_token = tokenize(text);
    return parse(list_token);
}

Term_t* reverse(Term_t* list) {
    Term_t* term = list;
    Term_t* result = _Nil();

    while(true) {
        switch(term->tag) {
            case CONS:
                result = _Cons(term->t1,result);
                term = term->t2;
                continue;
            case NIL:
                return result;
            default:
                error("reverse takes a list!\n");
        }
    }
}

Term_t* tokenize_iter(char** text);

Term_t* tokenize(char* text) {
    char* ptr = text;
    return tokenize_iter(&ptr);
}

Term_t* tokenize_iter(char** text) {
    Term_t* result = _Nil();
    char buf[64];
    int idx = 0;
    char* ptr = *text;
    ///////////////////
    eat_space:
    //printf("1:%s\n",ptr);
    switch(*ptr) {
        case '(':
            ptr ++;
            //printf("enter %s\n", ptr);
            result = _Cons(tokenize_iter(&ptr),result);
            //printf("return %s\n", ptr);
            goto eat_space;
        case ')':
        case '\0':
            ptr++;
            *text = ptr;
            return reverse(result);
        case ' ' :
        case '\n' :
        case '\t' :
            ptr ++;
            goto eat_space;
        default:
            goto token_out;
    }
    ///////////////////
    token_out:
    //printf("2:%s\n",ptr);
    buf[idx++] = *ptr++;
    switch(*ptr) {
        case '(':
        case ')':
        case ' ' :
        case '\n' :
        case '\t' :
        case '\0' : {
            buf[idx] = '\0';
            char* str = malloc((idx+1) * sizeof(char));
                        
            result = _Cons(_Symb(to_symb(buf)),result);
            idx = 0;
            goto eat_space;
        }
        default:
            goto token_out;
    }
    ///////////////////
}


bool token_lambda(char* str, char** result) {
    size_t len = strlen(str);
    str = 
    strncpy(str,label+1,len-2);
    Symb_t* new_symb = to_symb(str);
    free(str);

    
    if(str[0] == '\\' && str[len-1] == '.') {
        *result = malloc(len-1 * sizeof(char));

        return _Abs(new_symb,parse(term->t2));
    }
}

bool token_int(char* str, Int_t* result) {
    char* ptr = str;
    Int_t value = 0;
    if(*ptr == '-') {
        ptr ++;
    }

    while(*ptr++ != '\0') {
        if(isdigit(*ptr)) {
            continue;
        } else {
            return false;
        }

    }
    return true;
}

Term_t* parse(Term_t* arg_term) {
    Term_t* term = arg_term;
    Term_t* buffer[256];
    size_t idx = 0;

    show_term(term);
    printf("\n");

    if(term->tag == NIL) {
        return _Nil();
    } else if(term->tag == CONS && term->t2->tag == NIL) {
        return parse(term->t1);
    } else if(term->tag == CONS && term->t2->tag == CONS) {
        if(term->t1->tag != SYMB || term->t2->t1->tag != SYMB) {
            error("the content of list is not symbols");
        }



        //printf("!!\n");
        if(term->t1->tag == SYMB) {
            

            printf("here\n");
            if(strcmp(label,";") == 0) {
                printf("here\n");
                return parse(term->t2);
            }
        }


        if(term->t2->t2->tag != NIL) {
            Term_t* new_term = _App(parse(term->t1),
                parse(term->t2->t1));
            return parse(_Cons(new_term,term->t2->t2));
        } else {
            return _App(parse(term->t1),parse(term->t2->t1));
        }
    } else {
        return term;
    }
        
    }

    
}
