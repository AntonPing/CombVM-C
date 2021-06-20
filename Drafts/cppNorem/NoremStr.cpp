 #include <iostream>
using namespace std;

class String{
private:



    Term* fun;
    Term* arg;
public:
    App(Term* fun, Term* arg) {
        type = typeid(App).name();
        this->fun = fun;
        this->arg = arg;
    }
    Term* eval(Stack* stack); // implementation after stack
    string show() {
        return "(" + fun->show() + " " + arg->show() + ")";
    };
};

