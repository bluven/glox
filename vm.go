package main

import "fmt"

type InterpretResult int

const (
	InterpretOK InterpretResult = iota
	InterpretCompileError
	InterpretRuntimeError

	StackMax = 256
)

type VM struct {
	chunk          *Chunk
	ip             uint
	traceExecution bool

	stack    [StackMax]Value
	stackTop int
}

func NewVM(traceExecution bool) *VM {
	vm := &VM{
		traceExecution: traceExecution,
	}
	vm.init()
	return vm
}

func (vm *VM) init() {
	vm.resetStack()
}

func (vm *VM) resetStack() {
	vm.stackTop = 0
}

func (vm *VM) push(value Value) {
	vm.stack[vm.stackTop] = value
	vm.stackTop++
}

func (vm *VM) pop() Value {
	vm.stackTop--
	return vm.stack[vm.stackTop]
}

func (vm *VM) Free() {

}

func (vm *VM) Interpret(chunk *Chunk) InterpretResult {
	vm.chunk = chunk
	vm.ip = 0
	return vm.run()
}

func (vm *VM) run() InterpretResult {
	for {
		instruction := vm.readByte()
		if vm.traceExecution {
			fmt.Printf("          ")
			for i := 0; i < vm.stackTop; i++ {
				fmt.Printf("[ ")
				printValue(vm.stack[i])
				fmt.Printf(" ]")
			}
			fmt.Printf("\n")
			vm.chunk.disassembleInstruction(int(vm.ip - 1))
		}
		switch instruction {
		case OpConstant:
			cv := vm.readConstant()
			vm.push(cv)
			break
		case OpNegate:
			vm.push(-vm.pop())
		case OpAdd:
			vm.add()
		case OpSubtract:
			vm.subtract()
		case OpMultiply:
			vm.multiply()
		case OpDivide:
			vm.divide()
		case OpReturn:
			printValue(vm.pop())
			fmt.Printf("\n")
			return InterpretOK
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

func (vm *VM) add() {
	v1, v2 := vm.pop(), vm.pop()
	vm.push(v1 + v2)
}

func (vm *VM) subtract() {
	v1, v2 := vm.pop(), vm.pop()
	vm.push(v1 - v2)
}

func (vm *VM) multiply() {
	v1, v2 := vm.pop(), vm.pop()
	vm.push(v1 * v2)
}

func (vm *VM) divide() {
	v1, v2 := vm.pop(), vm.pop()
	vm.push(v2 / v1)
}
