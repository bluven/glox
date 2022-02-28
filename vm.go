package main

import "fmt"

type InterpretResult int

const (
	INTERPRET_OK InterpretResult = iota
	INTERPRET_COMPILE_ERROR
	INTERPRET_RUNTIME_ERROR
)

type VM struct {
	chunk          *Chunk
	ip             uint
	traceExecution bool
}

func NewVM(traceExecution bool) *VM {
	vm := &VM{
		traceExecution: traceExecution,
	}
	vm.init()
	return vm
}

func (vm *VM) init() {

}

func (vm *VM) Free() {

}

func (vm *VM) Interpret(chunk *Chunk) {
	vm.chunk = chunk
	vm.ip = 0
	vm.run()
}

func (vm *VM) run() InterpretResult {
	for {
		instruction := vm.readByte()
		if vm.traceExecution {
			vm.chunk.disassembleInstruction(int(vm.ip - 1))
		}
		switch instruction {
		case OpConstant:
			{
				cv := vm.readConstant()
				printValue(cv)
				fmt.Println("")
				break
			}
		case OpReturn:
			return INTERPRET_OK
		}
	}
}

func (vm *VM) readByte() OpCode {
	op := vm.chunk.codes[vm.ip]
	vm.ip += 1
	return op
}

func (vm *VM) readConstant() Value {
	ci := vm.readByte()
	return vm.chunk.constants[ci]
}
