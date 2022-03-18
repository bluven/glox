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

	vm := NewVM(*disassemble, *traceExecution)
	argc := len(flag.Args())
	if argc == 0 {
		repl(vm)
	} else if argc == 1 {
		runFile(vm, flag.Arg(0))
	} else {
		fmt.Fprintf(os.Stderr, "Usage: glox [path]\n")
		os.Exit(64)
	}

	vm.Free()
}

func repl(vm *VM) {
	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Printf("> ")
		scanner.Scan()

		// 加换行符是因为解析数字时，peek会越界，正常的statement应该是";"结尾的
		vm.Interpret(scanner.Text() + "\n")
	}
}

func runFile(vm *VM, filename string) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to open file: %s.\n", err)
		return
	}

	switch vm.Interpret(string(content)) {
	case InterpretCompileError:
		os.Exit(65)
	case InterpretRuntimeError:
		os.Exit(70)
	}
}
