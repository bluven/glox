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

	argc := len(flag.Args())
	if argc == 0 {
		repl()
	} else if argc == 1 {
		runFile(flag.Arg(0))
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

		// 加换行符是因为解析数字时，peek会越界，正常的statement应该是";"结尾的
		interpret(scanner.Text() + "\n")
	}
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

func interpret(source string) InterpretResult {
	chunk := NewChunk()

	if !compile(source, chunk, *disassemble) {
		chunk.Free()
		return InterpretCompileError
	}

	vm := NewVM(*traceExecution)
	result := vm.Interpret(chunk)
	chunk.Free()

	return result
}
