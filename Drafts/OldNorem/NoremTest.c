#include "Norem.h"

void test() {
    def_basic("iADD",iADD);
    def_basic("iSUB",iSUB);
    def_basic("iPRINT",iPRINT);
    def_basic("IFTE",IFTE);
    def_basic("EQL",EQL);
    def_basic("EXIT",EXIT);

    def_symb("+",compile(
        "\\x. \\y. x \\a. y \\b. iADD a b"));
    def_symb("-",compile(
        "\\x. \\y. x \\a. y \\b. iSUB a b"));
    def_symb("==",compile(
        "\\x. \\y. x \\a. y \\b. EQL a b"));
    def_symb("if",compile(
        "\\p. p IFTE"));
    def_symb("swap",compile(
        "\\x. \\y. y x"));
    def_symb("test2",compile(
        "(`+ `(+ 1 2) `(+ 3 4)) `iPRINT EXIT"));

    def_symb("fib",compile(
        "\\x. `if (== x 0) 1 ; \
               if (== x 1) 1 ; \
                + (fib ; - x 1) \
                  (fib ; - x 2)"));
    show_dict();
    return;
}

void test_parse() {
    Term_t* term;
    term = compile("\\x. wwwe; fdfdfd \\y. y x");
    printf("ok\n");
    show_term(term);
    printf("\n");
}

void run_repl() {
    static Char_t input[2048];
    Term_t* instr;
    puts("Norem version 0.1");
    puts("Press Ctrl_c to Exit\n");
    while(1){
        fputs("> ",stdout);
        fgets(input, 2048, stdin);
        instr = compile(input);
        show_term(instr);
        printf("\n");
        eval(App(App(instr,Var(to_symb("iPRINT"))),
            Var(to_symb("EXIT"))));
    }
}


int main() {
    heap_init();
    stack_init();
    dict_init();
    
    //show_term(compile("iADD 1 2 (iPRINT EXIT)"));
    test();
    run_repl();
    //test_parse();
    //run_repl();

    return 0;
}