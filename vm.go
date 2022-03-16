package main

import (
	"fmt"
	"os"
)

type InterpretResult int

const (
	InterpretOK InterpretResult = iota
	InterpretCompileError
	InterpretRuntimeError

	StackMax = 256
)

type BinaryOp func(v1, v2 float64) interface{}

var (
	opAdd      = func(v1, v2 float64) interface{} { return v1 + v2 }
	opSubtract = func(v1, v2 float64) interface{} { return v1 - v2 }
	opMultiply = func(v1, v2 float64) interface{} { return v1 * v2 }
	opDivide   = func(v1, v2 float64) interface{} { return v1 / v2 }
	opGreater  = func(v1, v2 float64) interface{} { return v1 > v2 }
	opLess     = func(v1, v2 float64) interface{} { return v1 < v2 }
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

func (vm *VM) peek(distance int) Value {
	return vm.stack[vm.stackTop-distance-1]
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
				vm.stack[i].Print()
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
			if vm.peek(0).IsNumber() {
				vm.push(numberValue(-vm.pop().Number()))
			} else {
				vm.runtimeError("Operand must be a number.")
				return InterpretRuntimeError
			}
		case OpAdd:
			if result := vm.binaryOp(opAdd, numberValue); result != InterpretOK {
				return result
			}
		case OpSubtract:
			if result := vm.binaryOp(opSubtract, numberValue); result != InterpretOK {
				return result
			}
		case OpMultiply:
			if result := vm.binaryOp(opMultiply, numberValue); result != InterpretOK {
				return result
			}
		case OpDivide:
			if result := vm.binaryOp(opDivide, numberValue); result != InterpretOK {
				return result
			}
		case OpTrue:
			vm.push(boolValue(true))
		case OpFalse:
			vm.push(boolValue(false))
		case OpNil:
			vm.push(nilValue())
		case OpNot:
			vm.push(boolValue(vm.pop().IsFalsey()))
		case OpEqual:
			b, a := vm.pop(), vm.pop()
			vm.push(boolValue(a.Equal(b)))
		case OpGreater:
			if result := vm.binaryOp(opGreater, boolValue); result != InterpretOK {
				return result
			}
		case OpLess:
			if result := vm.binaryOp(opLess, boolValue); result != InterpretOK {
				return result
			}
		case OpReturn:
			vm.pop().Print()
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

func (vm *VM) binaryOp(op BinaryOp, valueFn ValueFn) InterpretResult {
	if vm.peek(0).IsNumber() && vm.peek(1).IsNumber() {
		v2, v1 := vm.pop().Number(), vm.pop().Number()
		vm.push(valueFn(op(v1, v2)))
		return InterpretOK
	} else {
		vm.runtimeError("Operands must be numbers.")
		return InterpretRuntimeError
	}
}

func (vm *VM) runtimeError(format string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, format, args...)
	fmt.Fprintf(os.Stderr, "\n")

	line := vm.chunk.lines[vm.ip-1]
	fmt.Fprintf(os.Stderr, "[line %d] in script\n", line)
	vm.resetStack()
}
