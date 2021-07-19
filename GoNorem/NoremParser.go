package main

import (
	"fmt"
    "strings"
    "io"
)

func isDigit(c rune) bool {
    return c >= '0' && c <= '9';
}
func isAlpha(c rune) bool {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}
func isSpace(c rune) bool {
    return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}
func isExtended(c rune) bool {
    list := "+-*/<=>!?:$%%_&~^";
    return strings.ContainsRune(list, c);
}
func isLegal(c rune) bool {
    return isAlpha(c) || isDigit(c) || isExtended(c);
}
func isParen(c rune) bool {
    list := "()[]{}";
    return strings.ContainsRune(list, c);
}
func isDelim(c rune) bool {
	list := ";,.\\";
    return strings.ContainsRune(list, c);
}

type struct Parser {
    text string
    
}




func (r *io.Reader) ReadEOF() (err error) {
    ch, _, err := r.ReadRune()
    if err == io.EOF {
        return nil // Ok
    } else if err != nil{
        return err
    } else {
        return error.New("Not EOF")
    }
}



type Term interface {
    TypeName() string
	Show() string
	Parse(string) string
}


type DInt struct {
	value int64
}

func (this *DInt) TypeName() string {
	return "Int"
}

func (this *DInt) Show() string {
	return fmt.Sprintf("%d", this.value)
}


