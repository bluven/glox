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
	traceExecution bool
	disassemble    bool

	chunk   *Chunk
	ip      uint
	objects *Object
	globals map[string]Value

	stack    [StackMax]Value
	stackTop int
}

func NewVM(disassemble, traceExecution bool) *VM {
	vm := &VM{
		traceExecution: traceExecution,
		disassemble:    disassemble,
	}
	vm.init()
	return vm
}

func (vm *VM) init() {
	vm.resetStack()
}

func (vm *VM) resetStack() {
	vm.stackTop = 0
	vm.globals = make(map[string]Value)
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
	vm.globals = nil
	vm.freeObjects()
}

func (vm *VM) Interpret(source string) InterpretResult {
	chunk, ok := NewParser(source, vm.disassemble).compile()
	if !ok {
		return InterpretCompileError
	}

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
			if vm.peek(0).IsString() && vm.peek(1).IsString() {
				v2, v1 := vm.pop(), vm.pop()
				vm.push(stringValue(v1.String() + v2.String()))
			} else if result := vm.binaryOp(opAdd, numberValue); result != InterpretOK {
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
		case OpPrint:
			vm.pop().Print()
			fmt.Println()
		case OpPop:
			vm.pop()
		case OpGetGlobal:
			name := vm.readString()
			if value, ok := vm.globals[name]; ok {
				vm.push(value)
			} else {
				vm.runtimeError("Undefined variable '%s'.", name)
				return InterpretRuntimeError
			}
		case OpSetGlobal:
			name := vm.readString()
			value := vm.peek(0)

			if _, ok := vm.globals[name]; ok {
				vm.globals[name] = value
			} else {
				vm.runtimeError("Undefined variable '%s'.", name)
				return InterpretRuntimeError
			}
		case OpDefineGlobal:
			name := vm.readString()
			vm.globals[name] = vm.peek(0)
			vm.pop()
			break
		case OpGetLocal:
			index := vm.readByte()
			vm.push(vm.stack[index])
		case OpSetLocal:
			index := vm.readByte()
			vm.stack[index] = vm.peek(0)
		case OpJumpIfFalse:
			offset := vm.readShort()
			if vm.peek(0).IsFalsey() {
				vm.ip += uint(offset)
			}
		case OpJump:
			vm.ip += uint(vm.readShort())
		case OpLoop:
			vm.ip -= uint(vm.readShort())
		case OpReturn:
			return InterpretOK
		}
	}
}

func (vm *VM) allocateObject(v interface{}) *Object {
	switch v.(type) {
	case string:
		obj := &Object{Type: ObjectString, Data: v, Next: vm.objects}
		vm.objects = obj
		return obj
	default:
		return nil
	}
}

func (vm *VM) freeObjects() {
	for vm.objects != nil {
		_ = vm.objects
		// todo: freeObject
		vm.objects = vm.objects.Next
	}
}

func (vm *VM) readByte() OpCode {
	op := vm.chunk.codes[vm.ip]
	vm.ip += 1
	return op
}

func (vm *VM) readShort() uint16 {
	vm.ip += 2
	return uint16(vm.chunk.codes[vm.ip-2]<<8 | vm.chunk.codes[vm.ip-1])
}

func (vm *VM) readConstant() Value {
	ci := vm.readByte()
	return vm.chunk.constants[ci]
}

func (vm *VM) readString() string {
	return vm.readConstant().String()
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
