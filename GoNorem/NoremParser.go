package main

import (
	"errors"
	"regexp"
	"strconv"
	//"strings"
	//"fmt"
)


type Input struct {
	text []string
	pos int
	stack []int
	pass bool
}

func (inp *Input) Push() {
	inp.stack.append(inp.pos)
}

func (inp *Input) Pop() {
	length := len(inp.stack)
	inp.pos = inp.stack[length-1]
	inp.stack = inp.stack[length-1:]
}

/*
func (inp *Input) [T any]And(func) bool {
	if inp.pass {
		return true
	} else {
		inp.Pop()
		return false
	}
}
*/


func Tokenize(str string) Input {
	results := make([]string, 0, 1)
	// Work around lack of quoting in backtick
	re := regexp.MustCompile(
        `[\s]*([\[\]{}()]|[^\s]*)`)
	for _, group := range re.FindAllStringSubmatch(str, -1) {
		if (group[1] == "") {
			continue
		}
		results = append(results, group[1])
	}
	return Input{results,0,nil,false}
}

/*

func ParseInt(inp *Input) (int,error) {
	backup := *inp

	const reInt = MustCompile(`^-?[0-9]+$`)
	r1, err := RegMatch(inp, reInt)
	if err != nil { goto parse_int_fail }
	r2, err := strconv.Atoi(r);
	if err != nil { panic("Cannot convert string to int!") }
	// success
	return DInt(r2), nil

	parse_int_fail:
	*inp = backup
	return 0, errors.New("Parse Int Failed")
}


func (inp *Input) EOI() bool {
	return len(inp.text) == inp.pos
}


}

type Term interface {
    Type() string
}

type DInt int64

func (d DInt) Type() string {
	return "Int"
}





func ParseTerm(inp *Input) (Term,error) {
	backup := *inp

	r1, err := ParseInt(inp)
	if err == nil { return r1,nil }


	parse_term_fail:
	*inp = backup
	return 0, errors.New("Parse Int Failed")
}



*/

/*
func (inp *Input) Next() *string {
	if inp.position >= len(inp.tokens) {
		return nil
	}
	token := inp.tokens[inp.position]
	inp.position ++
	return &token
}

func (inp *Input) Peek() *string {
	if inp.position >= len(inp.tokens) {
		return nil
	}
	return &inp.tokens[inp.position]
}

func (inp *Input) Bind() *string {
	if inp.position >= len(inp.tokens) {
		return nil
	}
	token := inp.tokens[inp.position]
	inp.position ++
	return &token
}




func (inp *Input) ReadTerm() (Term,error) {
	token := inp.Next()
	if token == nil {
		return nil, errors.New("EOF")
	}

    match, _ := regexp.MatchString(`^-?[0-9]+$`, *token);
	if match {
		i, e := strconv.Atoi(*token);
		if e != nil {
			return nil, errors.New("Int parse error")
		}
		return DInt{i}, nil
	}

    match, _ := regexp.MatchString(`^[a-z]*$`, *token);
	if match {
		i, e := strconv.Atoi(*token);
		if e != nil {
			return nil, errors.New("Int parse error")
		}
		return DInt{i}, nil
	}

    //no_match:
    return nil, errors.New("No match")
}







type App struct {
    t1 Term
    t2 Term
}

type Token struct {
    name string
}




*/