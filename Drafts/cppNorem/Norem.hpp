#ifndef NOREM_H
#define NOREM_H
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <iostream>
#include <stack>
#include <vector>

using namespace std;



class Stack;

class Term {
public:
    string type;
    Term() {
        type = typeid(T)
    }
    virtual Term* eval(Stack* stack) = 0;
    virtual string show() = 0;
    static Term* read(Parser* par);
    template<typename T> T* cast() {
        if(this->type == typeid(T).name()) {
            return static_cast<T*>(this);
        } else {
            return nullptr;
        }
    }
};

class StatTerm : Term {
public:
    virtual void init() = 0;
    virtual Term* get() = 0;
}

// S is a singleton
class S_comb : public StatTerm {
    static instance;
public:
    init() { instance = S_comb; }
    get()  { return instance; }


    
    S_comb() {
        type = typeid(S_comb).name();
    };
    Term* eval(Stack* stack) {
        Term* f = stack->pop();
        Term* g = stack->pop();
        Term* x = stack->pop();
        stack->push(new App(g,x));
        stack->push(x);
        return f;
    }
    string show() {
        return "S";
    }
};








class Stack {
private:
    size_t capacity;
    Term** stack_base;
    Term** stack_ceil;
    Term** pointer;
public:
    Stack(size_t cap) {
        capacity = cap;
        stack_base = (Term**)malloc(sizeof(Term*)*cap);
        stack_ceil = stack_base + cap;
        pointer = stack_base;
    }
    void push(Term* term) {
        if(pointer != stack_ceil) {
            *pointer = term;
            pointer ++;
        } else {
            cout << "stack overflow!" << endl;
        }
    }
    Term* pop() {
        if(pointer != stack_base) {
            pointer --;
            return *pointer;
        } else {
            cout << "pop null" << endl;
            return nullptr;
        }
    }
    bool is_empty() {
        return pointer == stack_base;
    }
    Term* rewind() {
        Term* result;
        Term** temp_ptr = pointer;
        if(temp_ptr == stack_base) {
            return nullptr;
        } else {
            //cout << "here" << endl;
            temp_ptr --;
            result = *temp_ptr;
        }
        while(temp_ptr != stack_base) {
            temp_ptr --;
            result = new App(result,*temp_ptr);
        }
        //cout << "rewind:" << result->show() << endl;
        return result;
    }
};

// Now, implementaion!
Term* App::eval(Stack* stack) {
    stack->push(arg);
    return fun;
}


class Task {
private:
    Term* with;
    Stack* stack;
public:
    Task() {
        with = nullptr;
        stack = new Stack(2048);
    }
    void run_step() {
        show_state();
        with = with->eval(stack);
    }
    void run(Term* term) {
        with = term;
        while(with != nullptr) {
            run_step();
        }
    }
    void show_state() {
        Term* temp;
        if(with != nullptr) {
            //cout << "1" << with->show() << endl;
            stack->push(with);
            temp = stack->rewind();
            with = stack->pop();
            //cout << "2" << with->show() << endl;
        } else {
            temp = stack->rewind();
        }

        if(temp == nullptr) {
            cout << "empty task!" << endl;
        } else {
            cout << temp->show() << endl;
        }
    }
    void kill() {
        cout << "task killed" << endl;
        delete this;
    }
};

class Var : public Term {
private:
    string x;
public:
    Var(string x) {
        type = typeid(Var).name();
        this->x = x;
    }
    bool match(string x) {
        return this->x == x;
    }
    Term* eval(Stack* stack) {
        stack->push(this);
        return nullptr;
    }
    string show() {
        return x;
    }
    static Term* read(Parser* par) {
        clog << "Var read:" << par->text << endl;

        par->record();
        while(par->read_space()) {};

        if(!par->read_char((char*)"abcde")) {
            clog << "Var read failed:" << par->text << endl;
            par->undo();
            return nullptr;
        }
        while(par->read_char((char*)"abcde")) {}

        char* var_str = par->cut_slice();
        string str = var_str;
        return new Var(str);
    }
};


class Lambda : public Term {
private:
    string x;
    Term* t;
public:
    Lambda(string x, Term* t) {
        type = typeid(Lambda).name();
        this->x = x;
        this->t = t;
    }
    Term* eval(Stack* stack) {
        Term* arg = stack->pop();
        return this;
    }
    string show() {
        return "λ" + x + "." + t->show();
    }
};

class Int : public Term {
public:
    int64_t value;
    Int(int64_t value) {
        type = typeid(Int).name();;
        this->value = value;
    }
    Term* eval(Stack* stack) {
        stack->push(this);
        return nullptr;
    }
    string show() {
        char* x = new char[64];
        sprintf(x, "%ld", value);
        string str = x;
        delete[] x;
        return str;
    }
    static Term* read(Parser* par) {
        par->record(); // 0
        clog << "Int read:" << par->text << endl;
        while(par->read_space()) {};

        par->read_char((char*)"-");
        if(!par->read_char((char*)"0123456789")) {
            par->undo();
            clog << "Int read failed:" << par->text << endl;
            return nullptr;
        }
        while(par->read_char((char*)"0123456789")) {}

        char* num_str = par->cut_slice();
        int64_t num;
        
        if(sscanf(num_str,(char*)"%ld",&num)) {
            clog << "Int read matched:" << num_str << endl;
            free(num_str);
            return new Int(num);
        } else {
            clog << "Int read failed:" << par->text << endl;
            return nullptr;
        }
    }

};



// K is a singleton
class K_comb : public Term {
public:
    K_comb() {
        type = typeid(K_comb).name();
    };
    Term* eval(Stack* stack) {
        Term* x = stack->pop();
        Term* y = stack->pop();
        return x;
    }
    string show() {
        return "K";
    }
};

// I is a singleton
class I_comb : public Term {
public:
    I_comb() {
        type = typeid(I_comb).name();
    };
    Term* eval(Stack* stack) {
        Term* x = stack->pop();
        return x;
    }
    string show() {
        return "I";
    }
};

// B is a singleton
class B_comb : public Term {
public:
    B_comb() {
        type = typeid(B_comb).name();
    };
    Term* eval(Stack* stack) {
        Term* f = stack->pop();
        Term* g = stack->pop();
        Term* x = stack->pop();
        stack->push(new App(g,x));
        return f;
    }
    string show() {
        return "B";
    }
};

// C is a singleton
class C_comb : public Term {
public:
    C_comb() {
        type = typeid(C_comb).name();
    };
    Term* eval(Stack* stack) {
        Term* f = stack->pop();
        Term* g = stack->pop();
        Term* x = stack->pop();
        stack->push(g);
        stack->push(x);
        return f;
    }
    string show() {
        return "C";
    }
};

Term* SC;
Term* KC;
Term* IC;
Term* BC;
Term* CC;
Term* ADD;

class Add_fn : public Term {
public:
    Add_fn() {
        type = typeid(Add_fn).name();
    };
    Term* eval(Stack* stack) {
        cout << "ADD" << endl;
        Term* x = stack->pop();
        Term* y = stack->pop();

        if(x->cast<Int>() != nullptr) {
            if(y->cast<Int>() != nullptr) {
                return new Int(
                    x->cast<Int>()->value +
                    y->cast<Int>()->value
                );
            } else {
                goto undo;
            }
        } else {
            goto undo;
        }

        undo:
        cout << "ADD fail" << endl;
        stack->push(y);
        stack->push(x);
        stack->push(this);
        return nullptr;
    }
    string show() {
        return "+";
    }
    static Term* read(Parser* par) {
        par->record();
        while(par->read_space()) {};
        if(par->read_char((char*)"+")) {
            return ADD;
        } else {
            par->undo();
            return nullptr;
        }
    }
};


Term* Term::read(Parser* par) {
    Term* result;

    clog << "term read:" << par->text << endl;

    par->record();
    while(par->read_space()) {};
    if(par->read_char((char*)"(")) {
        result = App::read(par);
        if(result != nullptr) {
            if(par->read_char((char*)")")) {
                clog << "a nested term!" << endl;
                return result;
            }
        }
    }
    par->undo();

    clog << "term try Int:" << par->text << endl;
    result = Int::read(par);
    if(result != nullptr) {
        return result;
    }

    clog << "term try Var:" << par->text << endl;
    result = Var::read(par);
    if(result != nullptr) {
        return result;
    }

    clog << "term try Add:" << par->text << endl;
    result = Add_fn::read(par);
    if(result != nullptr) {
        return result;
    }

    return nullptr;
}

void init_singleton() {
    SC = new S_comb();
    KC = new K_comb();
    IC = new I_comb();
    BC = new B_comb();
    CC = new C_comb();
    ADD = new Add_fn();
    return;
}

Term* parse(string text) {
    Term* result;
    Parser par = Parser(text);
    
    clog << "start paring... " << text << endl;

    par.record();
    result = Term::read(&par);
    if(result != nullptr) {
        while(par.read_space()) {};
        if(par.is_drained()) {
            return result;
        }
    }
    par.undo();
    
    clog << "not a Term, try App now!" << endl;

    par.record();
    result = App::read(&par);
    if(result != nullptr) {
        while(par.read_space()) {};
        if(par.is_drained()) {
            return result;
        }
    }
    par.undo();

    cout << "parse failed on: " << par.text << endl;
    return nullptr;
}

int main() {
    init_singleton();
    /*
    Term* a = parse<Int>("42");
    Term* b = parse<Int>("42");
    Term* f = parse<Add_fn>("+");
    */
    Term* test = parse("(+ (+ 1 2 aa) (+ 1 abb 2))");
    if(test == nullptr) { return 0; }


    Task my_task = Task();

    cout << "term=" << test->show() << endl;
    my_task.run(test);
    cout << "ok" << endl;
    
    //test.eval
    return 0;
}
#endif