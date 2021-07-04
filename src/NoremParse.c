#include "Norem.h"

/*
| Int
| Real
| Char: 'a-zA-z0-9'*
| Bool: True | False
| [t1,t2,...,tn]
| (t1 t2 ... tn)
| {t1 t2 ... tn}
| \x. t1 t2 ... tn
| (x -> t)
*/

typedef struct Parser_t {
    bool success;
    union {
        int_t int_v;
        real_t real_v;
        char_t char_v;
        char_t* string_v;
        // to be continued....
    };
    char_t* text_base;
    char_t* text_ptr;
} Parser_t;

#define PARSER_CHECK(p) \
    if(!p.success) return p

#define PARSER_BIND(p1,p2) \
    p2.success ? p2 : p1

#define PARSER_FAIL(p) \
    p.success = false; \
    return p

#define PARSER_SUCCESS(p) \
    p.success = true; \
    return p

Parser_t end_of_text(Parser_t par) {
    PARSER_CHECK(par);
    if(par.text_ptr[0] == '\0') {
        PARSER_SUCCESS(par);   
    } else {
        PARSER_FAIL(par);
    }
}

Parser_t read_char(Parser_t par) {
    PARSER_CHECK(par);
    if(par.text_ptr[0] != '\0') {
        par.char_v = par.text_ptr[0];
        par.text_ptr ++;
        PARSER_SUCCESS(par);
    } else {
        PARSER_FAIL(par);
    }
}

Parser_t parse_char(Parser_t par, char_t c) {
    PARSER_CHECK(par);
    Parser_t p1 = read_char(par);
    PARSER_CHECK(p1);
    if(p1.char_v == c) {
        PARSER_SUCCESS(p1);
    } else {
        PARSER_FAIL(p1);
    }
}

Parser_t char_satisify(Parser_t par, bool (*fun) (char_t)) {
    PARSER_CHECK(par);
    Parser_t p1 = read_char(par);
    PARSER_CHECK(p1);
    if(fun(p1.char_v)) {
        PARSER_SUCCESS(p1);
    } else {
        PARSER_FAIL(p1);
    }
}

Parser_t read_string(Parser_t par, size_t len) {
    PARSER_CHECK(par);
    char_t* str = malloc(sizeof(char_t) * len + 1);
    memset(str, '\0', len + 1);
    strncpy(str, par.text_ptr, len);
    if(strlen(str) < len) {
        free(str);
        PARSER_FAIL(par);
    } else {
        par.string_v = str;
        par.text_ptr += len;
        PARSER_SUCCESS(par);
    }
}

Parser_t parse_string(Parser_t par, string_t str) {
    PARSER_CHECK(par);
    Parser_t p1 = read_string(par,strlen(str));
    PARSER_CHECK(p1);
    if(strcmp(p1.string_v,str) == 0) {
        PARSER_SUCCESS(p1);
    } else {
        PARSER_FAIL(p1);
    }
}

bool chr_in_str(char_t c, char_t* str) {
    char_t* ptr = str;
    while(*ptr != '\0') {
        if(c == *ptr++) { return true; }
    }
    return false;
}
bool is_digit(char_t c) {
    return c >= '0' && c <= '9';
}
bool is_alpha(char_t c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}
bool is_space(char_t c) {
    return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}
bool is_extended(char_t c) {
    static char_t extended[] = "+-.*/<=>!?:$%%_&~^";
    return chr_in_str(c,extended);
}
bool is_legal(char_t c) {
    return is_alpha(c) || is_digit(c) || is_extended(c);
}
bool is_spec_char(char_t c) {
    static char_t specs[] = "()[]{};,";
    return chr_in_str(c,specs);
}

Parser_t eat_space(Parser_t par) {
    PARSER_CHECK(par);
    while(true) {
        Parser_t p1 = char_satisify(par, is_space);
        if(p1.success) {
            par = p1; continue;
        } else {
            PARSER_SUCCESS(par);
        }
    }
}

Parser_t parse_digit(Parser_t par) {
    return char_satisify(par,is_digit);
}

Parser_t parse_unsigned_int(Parser_t par) {
    PARSER_CHECK(par);
    // minimal - one digit
    char* ptr = par.text_ptr;
    if(!is_digit(*ptr)) {
        PARSER_FAIL(par);
    }
    // read digits until fail
    int_t value = *ptr++ - '0';
    while(is_digit(*ptr)) {
        value *= 10;
        value += *ptr++ - '0'; // convert '0'~'9' to 0~9
    }
    // return value
    par.text_ptr = ptr;
    par.int_v = value;
    PARSER_SUCCESS(par);
}

Parser_t parse_int(Parser_t par) {
    PARSER_CHECK(par);
    bool_t with_minus = false;
    if(par.text_ptr[0] == '-') {
        par.text_ptr ++;
        with_minus = true;
    }
    Parser_t p1 = parse_unsigned_int(par);
    PARSER_CHECK(p1);
    if(with_minus) {
        p1.int_v *= -1;
    }
    PARSER_SUCCESS(p1);
}


void debug_show_parser(Parser_t par) {
    if(par.success) {
        printf(par.text_ptr);
        printf(" as %ld", par.int_v);
        printf(", success\n");
    } else {
        printf("fail\n");
    }
}

void parser_test() {
    Parser_t par;
    char* text = "-2048sadfas";
    par.success = true;
    par.text_base = text;
    par.text_ptr = text;

    Parser_t p1 = parse_string(par,"-2048sa");
    debug_show_parser(p1);
}



/*
Term_t* read_symb() {
    char_t* old_ptr = text_ptr;

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

bool read_char(char_t c) {
    if(*text_ptr == c) {
        text_next();
        return true;
    } else {
        return false;
    }
}


Term_t* read_lambda() {
    Term_t* result;
    Term_t* temp;
    char_t* old_ptr = text_ptr;
    
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
    char_t* old_ptr = text_ptr;
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
    char_t* old_ptr;
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

Term_t* compile(char_t* text) {
    char_t* rest;
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

*/