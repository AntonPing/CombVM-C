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



class NRString {
    string text;

}




template <class T>
class Parser {
public:
    size_t idx;
    T obj;

    virtual bool parse(string str, size_t idx);
};


template <class T>
class Digit {
public:
    Digit(string str) {
        text


    }


}




class And : public Parser {
    size_t my_idx;
    vector<Parser> list;
    vector<Term*> result;

public:
    bool match(string str, size_t index) {
        size_t idx = *index;
        for(int i=0; i<list.size(); i++) {
            bool result = list[i].match(string,&idx);
            if(!result) { return false; }
        }
        *index = idx;
        return true;
    }
}

class Or : public Parser {
    vector<Parser> list;
public:
    bool match(string str, size_t* index) {
        size_t idx = *index;
        for(int i=0; i<list.size(); i++) {
            bool result = list[i].match(string,&idx);
            if(result) {
                *index = idx;
                return true;
            }
        }
        return false;
    }
}

template<typename T>
class Read<T> : public Parser {
public:
    bool match(string str, size_t* index) {
        T::read(str,&index);
    }


}

class Alpha : public Parser {



}



class Parser {
public:
    string text;
    size_t index;
    stack<size_t> pivot;

    Parser(string text) {
        this->text = str;
        this->index = 0;
        //this->pivot.push(str);
    }
    void record() {
        pivot.push(index);
    }

    void undo() {
        index = pivot.top();
        pivot.pop();
    }

    bool read_char(string list) {
        clog <<  "read char:" << list << endl;
        if(*text == '\0') { return false; }
        if(strchr(list,*text) != nullptr) {
            text ++;
            return true;
        } else {
            return false;
        }
    }

    bool read_space() {
        switch(text[index]) {
            case ' ' :
            case '\n':
            case '\r':
            case '\t':
                index ++;
                return true;
            default:
                return false;
        }
    }

    bool is_drained() {
        while()
        return *text == '\0';
    }

    char* cut_slice() {
        size_t len = text - pivot.top();
        if(len > 0) {
            char* str = (char*)malloc(sizeof(char)*len);
            memcpy(str,pivot.top(),len);
            str[len] = '\0';
            pivot.pop();
            return str;
        } else {
            cerr << "length slice < 0" << endl;
            return nullptr;
        }
    }

    void assign(Parser parser) {
        this->text = parser.text;
        this->pivot = parser.pivot;
    }

    

    
};

#endif
