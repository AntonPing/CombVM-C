#include "Norem.h"

/*
| Int | Real | Char
| [t1,t2,...,tn]
| (t1 t2 ... tn)
| \x. t1 t2 ... tn
| (x -> t)
*/
Char_t* text_base;
Char_t* text_ptr;

void text_next() {
    if(*text_ptr == '\0') {
        error("parse failed!");
    }
    text_ptr ++;
}

bool chr_in_str(Char_t c, Char_t* str) {
    Char_t* ptr = str;
    while(*ptr != '\0') {
        if(c == *ptr) {
            return true;
        }
        ptr ++;
    }
    return false;
}
bool is_digit(Char_t c) {
    return c >= '0' && c <= '9';
}
bool is_alpha(Char_t c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}
bool is_space(Char_t c) {
    return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}
bool is_legal_char(Char_t c) {
    static Char_t legals[] = "~!@#$^&*-_=+:/?<>%%";
    return is_alpha(c) || is_digit(c) || chr_in_str(c,legals);
}

bool is_spec_char(Char_t c) {
    static Char_t specs[] = "()[]{};,.";
    return chr_in_str(c,specs);
}

Int_t parse_digit(Char_t c) {
    return (Int_t)(c - '0');
}

void eat_space() {
    while(true) {
        if(is_space(*text_ptr)) {
            text_next();
            continue;
        } else if(text_ptr[0] == '#' && text_ptr[1] == '|') {
            while(!(text_ptr[0] == '|' && text_ptr[1] == '#')) {
                text_next();
            }
            continue;
        } else if(*text_ptr == '#') {
            while(!(is_space(*text_ptr) && *text_ptr != ' ')) {
                text_next();
            }
            continue;
        } else {
            break;
        }
    }
}

Term_t* read_int() {
    bool with_minus;
    Int_t value;
    Char_t* old_ptr = text_ptr;

    // may begin with a minus
    if(*text_ptr == '-') {
        with_minus = true;
        text_next();
    } else {
        with_minus = false;
    }
    // minimal - one digit
    if(is_digit(*text_ptr)) {
        value = parse_digit(*text_ptr);
        text_next();
    } else {
        text_ptr = old_ptr;
        return NULL;
    }
    // more digits
    while(is_digit(*text_ptr)) {
        value *= 10;
        value += parse_digit(*text_ptr);
        text_next();
    }
    // return with(out) minus
    if(with_minus) {
        return Int(-value);
    } else {
        return Int(value);
    }
}

Symb_t* make_symb(Char_t* from, Char_t* to) {
    Char_t buffer[64];
    size_t idx;
    for(idx = 0; from + idx <= to; idx ++) {
        buffer[idx] = from[idx];
    }
    buffer[idx] = '\0';
    return to_symb(buffer);
}

Term_t* read_symb() {
    Char_t* old_ptr = text_ptr;

    // minimal - one letter
    if(is_legal_char(*text_ptr)) {
        text_next();
    } else {
        text_ptr = old_ptr;
        printf("failed : %c\n",*text_ptr);
        return NULL;
    }
    // more letters
    while(is_legal_char(*text_ptr)) {
        text_next();
    }
    // make and return
    printf("match! ");
    show_term(Symb(make_symb(old_ptr,text_ptr-1)));
    printf("\n");
    return Var(make_symb(old_ptr,text_ptr-1));
}

Term_t* read_lambda();

Term_t* read_apply_list() {
    Term_t* result;
    Term_t* temp;
    printf("app_list: %s\n",text_ptr);

    // first may be term
    eat_space();
    result = read_term();
    if(result == NULL) {
        // or a lambda
        if(*text_ptr == '\\') {
            result = read_lambda();
            if(result == NULL) {
                return NULL;
            } else {
                return result;
            }
        } else {
            return NULL;
        }
    }

    // more terms
    while(true) {
        //printf("loop:%s\n",text_ptr);
        eat_space();
        //printf("eat:%s\n",text_ptr);

        if(*text_ptr == ')' || *text_ptr == '\0') {
            return result;
        }
        // semicolon
        if(*text_ptr == ';') {
            text_next();
            temp = read_apply_list();
            if(temp == NULL) {
                return NULL;
            } else {
                return App(result,temp);
            }   
        }
        // try lambda
        if(*text_ptr == '\\') {
            temp = read_lambda();
            if(temp == NULL) {
                return NULL;
            } else {
                return App(result,temp);
            }
        }

        temp = read_term();
        if(temp == NULL) {
            return NULL;
        } else {
            result = App(result,temp);
            continue;
        }   
    }
}
/*
bool read_char(Char_t c) {
    if(*text_ptr == c) {
        text_next();
        return true;
    } else {
        return false;
    }
}
*/

Term_t* read_lambda() {
    Term_t* result;
    Term_t* temp;
    Char_t* old_ptr = text_ptr;
    
    printf("lambda: %s\n",text_ptr);

    // first term
    eat_space();

    // match '\'
    if(*text_ptr != '\\') {
        text_ptr = old_ptr;
        return NULL;
    } else {
        text_next();
    }
    
    // match a symb
    Term_t* var = read_symb();
    if(var == NULL) {
        text_ptr = old_ptr;
        return NULL;
    }

    // match '.'
    if(*text_ptr != '.') {
        text_ptr = old_ptr;
        return NULL;
    } else {
        text_next();
    }

    // match a term
    Term_t* term = read_apply_list();
    if(term == NULL) {
        text_ptr = old_ptr;
        return NULL;
    }

    return Abs(var->as_symb,term);
}

Term_t* read_debug_term() {
    Term_t* result;
    Char_t* old_ptr = text_ptr;
    eat_space();

    if(*text_ptr != '`') {
        text_ptr = old_ptr;
        return NULL;
    } else {
        text_next();
    }

    result = read_term();
    if(result == NULL) {
        text_ptr = old_ptr;
        return NULL;
    } else {
        return Debug(result);
    }
}

Term_t* read_term() {
    Char_t* old_ptr;
    Term_t* result;
    printf("term: %s\n",text_ptr);

    old_ptr = text_ptr;
    eat_space();
    // try app_list
    if(*text_ptr == '(') {
        text_next();
        result = read_apply_list();
        if(result == NULL) {
            return NULL;
        }
        if(*text_ptr == ')') {
            text_next();
            return result;
        } else {
            return NULL;
        }
    }
    
    // try debug_term
    result = read_debug_term();
    if(result != NULL) { return result; }

    // try int
    result = read_int();
    if(result != NULL) { return result; }

    // try symb
    result = read_symb();
    //printf("try symb: ");
    //show_term(result);
    if(result != NULL) { return result; }
    
    // no match
    return NULL;
}

Term_t* compile(Char_t* text) {
    Char_t* rest;
    Term_t* result;
    text_base = text;
    text_ptr = text;
    result = read_apply_list();
    
    eat_space();
    if(*text_ptr == '\0') {
        return result;
    } else {
        printf("parse failed!\n");
        printf(text_ptr);
        printf("\n");
        return NULL;
    }
}