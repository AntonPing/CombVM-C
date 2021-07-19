package main

import (
	//"strings"
	"fmt"
	"os"
	"os/exec"
	"github.com/chzyer/readline"
)

func main() {
	fmt.Println("hello")
	i := DInt{42}
	fmt.Println(i.Show())

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
		fmt.Println(line)
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