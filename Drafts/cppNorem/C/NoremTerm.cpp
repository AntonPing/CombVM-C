#include <iostream>
using namespace std;

#define INT long long
#define REAL long long

typedef Term* (func_t) (vector<Term*>);

class Term {
public:
    bool isAtom;
    union Value {
        struct App {
            Term* fun;
            Term* arg; 
        } app_v;
        uint64_t int_v;
        double real_v;
        func_t func_v;
    } value;
};

bool Term::isApp() {
    if(value.app_v.arg = NULL) {

    }
}

class Task {
    Term* top;
    vector<Term*> stack;


}

void Task::run() {
    func_t func;
    Term* new_top;
    loop:
    if(top.isAtom) {
        func = top.get_func();
        func(stack);
        goto loop:
    } else {
        stack.push(top);
        top = top.value.app_v.fun;
        goto loop;
    }
}

int main() {
}