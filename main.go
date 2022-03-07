package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
)

var (
	disassemble    = flag.Bool("disassemble", false, "")
	traceExecution = flag.Bool("trace-execution", false, "")
)

func main() {
	flag.Parse()

	argc := len(os.Args)
	if argc == 1 {
		repl()
	} else if argc == 2 {
		runFile(os.Args[1])
	} else {
		fmt.Fprintf(os.Stderr, "Usage: glox [path]\n")
		os.Exit(64)
	}

	vm := NewVM(*traceExecution)
	vm.Free()
}

func repl() {
	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Printf("> ")
		scanner.Scan()

		interpret(scanner.Text())
	}
}

func interpret(source string) InterpretResult {
	compile(source)
	return InterpretOK
}

func runFile(filename string) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to open file: %s.\n", err)
		return
	}

	switch interpret(string(content)) {
	case InterpretCompileError:
		os.Exit(65)
	case InterpretRuntimeError:
		os.Exit(70)
	}
}

func compile(source string) {
	scanner := NewScanner(source)
	line := -1

	for {
		token := scanner.Scan()
		if token.Line != line {
			fmt.Printf("%4d ", token.Line)
			line = token.Line
		} else {
			fmt.Printf("   | ")
		}
		fmt.Printf("%2d '%s'\n", token.Type, token.Lexeme)

		if token.Type == EOF {
			break
		}
	}
}
