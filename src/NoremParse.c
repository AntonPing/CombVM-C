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
        bool_t bool_v;
        symb_t symb_v;
        string_t string_v;
        Term_t* term_v;
        char_t* message;
        // to be continued...
    };
    char_t* text_base;
    char_t* text_ptr;
} Parser_t;





void DEBUG_SHOW_PARSER(Parser_t par, Tag_t tag) {
    if(par.success) {
        printf("rest: %s,",par.text_ptr);
        printf("match:");
        switch(tag) {
            case INT:
                printf("%ld",par.int_v); break;
            case REAL:
                printf("%lf",par.real_v); break;
            case CHAR:
                printf("%c",par.char_v); break;
            case BOOL:
                printf(par.bool_v ? "True" : "False"); break;
            case SYMB:
                printf(par.symb_v); break;
            case LAMB:
                show_lamb(par.term_v); break;
            case NIL:
                printf("Nil"); break;
            case TERM:
                show_term(par.term_v); break;
            default:
                printf("??"); break;
        }
        printf(", success\n");
    } else {
        printf("fail: %s\n", par.message);
    }
}

#define __PARSER_DEBUG__
#define PARSER_CHECK(p) \
    if(!p.success) return p

#define PARSER_FAIL(p,msg) \
    p.text_ptr = par.text_ptr; \
    p.success = false; \
    p.message = msg; \
    return p

#define PARSER_SUCCESS(p) \
    p.success = true; \
    return p

Parser_t end_of_text(Parser_t par) {
    PARSER_CHECK(par);
    if(par.text_ptr[0] == '\0') {
        PARSER_SUCCESS(par);
    } else {
        PARSER_FAIL(par,"end_of_text failed.");
    }
}

Parser_t read_char(Parser_t par) {
    PARSER_CHECK(par);
    if(par.text_ptr[0] != '\0') {
        par.char_v = par.text_ptr[0];
        par.text_ptr ++;
        PARSER_SUCCESS(par);
    } else {
        PARSER_FAIL(par,"read_char failed.");
    }
}

Parser_t parse_char(Parser_t par, char_t c) {
    PARSER_CHECK(par);
    Parser_t p1 = read_char(par);
    PARSER_CHECK(p1);
    if(p1.char_v == c) {
        PARSER_SUCCESS(p1);
    } else {
        PARSER_FAIL(p1,"parse_char failed.");
    }
}

Parser_t char_satisify(Parser_t par, bool (*fun) (char_t)) {
    PARSER_CHECK(par);
    Parser_t p1 = read_char(par);
    PARSER_CHECK(p1);
    if(fun(p1.char_v)) {
        PARSER_SUCCESS(p1);
    } else {
        PARSER_FAIL(p1,"char_satisfity failed.");
    }
}

Parser_t read_string(Parser_t par, size_t len) {
    PARSER_CHECK(par);
    char_t* str = substr(par.text_ptr,len);
    if(strlen(str) < len) {
        free(str);
        PARSER_FAIL(par,"read_string failed.");
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
        PARSER_FAIL(p1,"parse_string failed.");
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
    static char_t extended[] = "+-*/<=>!?:$%%_&~^";
    return chr_in_str(c,extended);
}
bool is_legal(char_t c) {
    return is_alpha(c) || is_digit(c) || is_extended(c);
}
bool is_spec_char(char_t c) {
    static char_t specs[] = "()[]{};,.\\";
    return chr_in_str(c,specs);
}

Parser_t parse_digit(Parser_t par) {
    Parser_t p1 = char_satisify(par,is_digit);
    if(p1.success) {
        PARSER_SUCCESS(p1);
    } else {
        PARSER_FAIL(p1,"char_satisfity failed.");
    }
}

Parser_t parse_unsigned_int(Parser_t par) {
    PARSER_CHECK(par);
    // minimal - one digit
    char* ptr = par.text_ptr;
    if(!is_digit(*ptr)) {
        PARSER_FAIL(par,"parse_unsigned_int failed.");
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

Parser_t parse_symb(Parser_t par) {
    PARSER_CHECK(par);
    // minimal - one legal
    char* ptr = par.text_ptr;
    if(!is_legal(*ptr)) {
        PARSER_FAIL(par,"parse_symb failed.");
    }
    // read legal until fail
    while(is_legal(*ptr)) {
        ptr ++;
    }
    string_t res = substr(par.text_ptr, ptr - par.text_ptr);
    printf("symbol! %s111\n",res);
    par.text_ptr = ptr;
    par.symb_v = to_symb(res);
    //free(res);
    PARSER_SUCCESS(par);
}

Parser_t parse_any_space(Parser_t par) {
    PARSER_CHECK(par);
    char* ptr = par.text_ptr;
    while(is_space(*ptr)) {
        par.char_v = *ptr;
        ptr ++;
    }
    par.text_ptr = ptr;
    PARSER_SUCCESS(par);
}

Parser_t parse_some_space(Parser_t par) {
    PARSER_CHECK(par);
    // minimal - one space
    char* ptr = par.text_ptr;
    if(!is_space(*ptr)) {
        PARSER_FAIL(par,"parse_some_space failed.");
    }
    return parse_any_space(par);
}


Parser_t parse_term(Parser_t par);
Parser_t parse_app_list(Parser_t par);

Parser_t parse_app_list(Parser_t par) {
    PARSER_CHECK(par);
    // minimal - one term
    par = parse_term(par);
    Term_t* with = par.term_v;
    if(!par.success) {
        PARSER_FAIL(par,"parse_app_list failed.");
    }
    // read term until fail
    Parser_t p1,p2;
    while(true) {
        p1 = parse_any_space(par);
        p1 = parse_term(p1);
        if(p1.success) {
            with = new_app(with, p1.term_v);
            par = p1; continue;
        } else {
            break;
        }
    }
    // semicolon
    p2 = parse_any_space(par);
    p2 = parse_char(p2,';');
    if(p2.success) { // semicolon notation!
        p2 = parse_app_list(p2);
        PARSER_CHECK(p2);
        p2.term_v = new_app(with,p2.term_v);
        PARSER_SUCCESS(p2);
    } else { // return term
        par.term_v = with;
        PARSER_SUCCESS(par);
    }
}

Parser_t parse_operator(Parser_t par) {
    PARSER_CHECK(par);
    
    // minimal - one non-space
    char* ptr = par.text_ptr;
    if(is_space(*ptr)) {
        PARSER_FAIL(par,"parse_operator failed.");
    }
    // read non-space until fail
    while(!is_space(*ptr)) {
        ptr ++;
    }
    string_t res = substr(par.text_ptr, ptr - par.text_ptr);
    par.text_ptr = ptr;

    if(strcmp(res,"+") == 0) {
        par.term_v = &tags[ADDI];
        PARSER_SUCCESS(par);
    } else if(strcmp(res,"printi") == 0) {
        par.term_v = &tags[PRINTI];
        PARSER_SUCCESS(par);
    } else if(strcmp(res,"exit") == 0) {
        par.term_v = &tags[EXIT];
        PARSER_SUCCESS(par);
    } else {
        PARSER_FAIL(par,"parse_operator failed.");
    }
}

Parser_t parse_term(Parser_t par) {
    PARSER_CHECK(par);
    Parser_t p1;

    p1 = parse_operator(par);
    if(p1.success) {
        DEBUG_SHOW_PARSER(p1,TERM);
        PARSER_SUCCESS(p1);
    }

    p1 = parse_int(par);
    if(p1.success) {
        DEBUG_SHOW_PARSER(p1,INT);
        p1.term_v = new_int(p1.int_v);
        PARSER_SUCCESS(p1);
    }

    p1 = parse_symb(par);
    if(p1.success) {
        DEBUG_SHOW_PARSER(p1,SYMB);
        p1.term_v = new_symb(p1.symb_v);
        PARSER_SUCCESS(p1);
    }

    p1 = parse_char(par,'(');
    p1 = parse_any_space(p1);
    p1 = parse_app_list(p1);
    Term_t* term = p1.term_v;
    p1 = parse_any_space(p1);
    p1 = parse_char(p1,')');
    if(p1.success) {
        p1.term_v = term;
        DEBUG_SHOW_PARSER(p1,TERM);
        PARSER_SUCCESS(p1);
    }

    p1 = parse_char(par,'\\');
    p1 = parse_symb(p1);
    symb_t x = p1.symb_v;
    p1 = parse_char(p1,'.');
    p1 = parse_any_space(p1);
    p1 = parse_app_list(p1);
    Term_t* t = p1.term_v;
    if(p1.success) {
        p1.term_v = new_lamb(x,t);
        DEBUG_SHOW_PARSER(p1,TERM);
        PARSER_SUCCESS(p1);
    }

    // no match
    PARSER_FAIL(par,"parse_term failed.");
}



void parser_test() {
    Parser_t par;
    char* text = "(\\x.\\y. + x y) 1 2";
    par.success = true;
    par.text_base = text;
    par.text_ptr = text;
    Parser_t p1 = parse_app_list(par);
    
    DEBUG_SHOW_PARSER(p1,TERM);
    //Term_t* res = ski_compile(p1.term_v);
    //show_term(res);
}