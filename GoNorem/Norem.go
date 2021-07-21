package main

import (
	"fmt"
	"os"
	"os/exec"
	"github.com/chzyer/readline"
)

func main() {
	fmt.Println("hello")

	rl, err := readline.NewEx(&readline.Config{
		Prompt:                 "> ",
		HistoryFile:            "/tmp/readline",
		DisableAutoSaveHistory: true,
	})
	if err != nil { panic(err) }
	defer rl.Close()

	for {
		line, err := rl.Readline()
		if err != nil { break }
		inp := Tokenize(line)
		res := ParseInt(&inp)
		fmt.Println(res)
	}
	fmt.Println("Goodbye!")
}

func run_editor() {
	cmd := exec.Command("vim", "test.txt")
	cmd.Stdin = os.Stdin
    cmd.Stdout = os.Stdout
	err := cmd.Run()
	if err != nil {
		fmt.Println("failed to call cmd.Run(): %v", err)
	}
}