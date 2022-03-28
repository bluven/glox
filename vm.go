package main

import (
	"fmt"
	"math"
	"os"
	"time"
)

type InterpretResult int

const (
	InterpretOK InterpretResult = iota
	InterpretCompileError
	InterpretRuntimeError

	FrameMax = 64
	StackMax = FrameMax * math.MaxUint8
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

	objects *Object
	globals map[string]Value

	openUpvalues *RuntimeUpvalue

	stack    [StackMax]Value
	frames   []*CallFrame
	stackTop uint
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
	vm.globals = make(map[string]Value)
	vm.frames = make([]*CallFrame, 0, FrameMax)

	vm.newNative("clock", clockNative)
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

func (vm *VM) peek(distance uint) Value {
	return vm.stack[vm.stackTop-distance-1]
}

func (vm *VM) Free() {
	vm.globals = nil
	vm.freeObjects()
}

func (vm *VM) Interpret(source string) InterpretResult {
	fn, ok := NewParser(source, vm.disassemble).compile()
	if !ok {
		return InterpretCompileError
	}

	closure := newClosure(fn)
	vm.push(closureValue(closure))
	vm.call(closure, 0)
	return vm.run()
}

func (vm *VM) run() InterpretResult {
	for {
		instruction := vm.readByte()
		if vm.traceExecution {
			fmt.Printf("          ")
			for i := uint(0); i < vm.stackTop; i++ {
				fmt.Printf("[ ")
				vm.stack[i].Print()
				fmt.Printf(" ]")
			}
			fmt.Printf("\n")
			frame := vm.currentFrame()
			frame.closure.Function.Chunk.disassembleInstruction(int(frame.ip - 1))
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
		case OpCloseUpvalue:
			vm.closeUpvalues(vm.stackTop - 1)
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
			slot := vm.currentFrame().slot + vm.readByte()
			vm.push(vm.stack[slot])
		case OpSetLocal:
			slot := vm.currentFrame().slot + vm.readByte()
			vm.stack[slot] = vm.peek(0)
		case OpGetUpValue:
			slot := vm.readByte()
			upvalue := vm.currentClosure().Upvalues[slot]

			var value Value
			if upvalue.Closed {
				value = upvalue.Value
			} else {
				value = vm.stack[upvalue.Location]
			}
			vm.push(value)
		case OpSetUpValue:
			upvalue := vm.currentClosure().Upvalues[vm.readByte()]
			if upvalue.Closed {
				upvalue.Value = vm.peek(0)
			} else {
				vm.stack[upvalue.Location] = vm.peek(0)
			}
		case OpJumpIfFalse:
			offset := vm.readShort()
			if vm.peek(0).IsFalsey() {
				vm.currentFrame().ip += uint(offset)
			}
		case OpJump:
			vm.currentFrame().ip += uint(vm.readShort())
		case OpLoop:
			vm.currentFrame().ip -= uint(vm.readShort())
		case OpCall:
			argCount := vm.readByte()
			if !vm.callValue(vm.peek(argCount), argCount) {
				return InterpretRuntimeError
			}
		case OpClosure:
			closure := newClosure(vm.readConstant().Function())
			vm.push(closureValue(closure))
			for i := range closure.Upvalues {
				isLocal := vm.readByte()
				index := vm.readByte()
				if isLocal == 1 {
					closure.Upvalues[i] = vm.captureUpvalue(vm.currentFrame().slot + index)
				} else {
					closure.Upvalues[i] = vm.currentFrame().closure.Upvalues[index]
				}
			}
		case OpReturn:
			result := vm.pop()

			frame := vm.frames[len(vm.frames)-1]
			vm.closeUpvalues(frame.slot)

			vm.frames = vm.frames[:len(vm.frames)-1]

			if len(vm.frames) == 0 {
				vm.pop()
				return InterpretOK
			}

			vm.stackTop = frame.slot
			vm.push(result)
		}
	}
}

func (vm *VM) closeUpvalues(last uint) {
	for vm.openUpvalues != nil && vm.openUpvalues.Location >= last {
		upvalue := vm.openUpvalues
		upvalue.Value = vm.stack[upvalue.Location]
		upvalue.Closed = true
		vm.openUpvalues = upvalue.Next
	}
}

func (vm *VM) captureUpvalue(local uint) *RuntimeUpvalue {
	var prevUpvalue *RuntimeUpvalue
	upvalue := vm.openUpvalues
	for upvalue != nil && upvalue.Location > local {
		prevUpvalue = upvalue
		upvalue = upvalue.Next
	}

	if upvalue != nil && upvalue.Location == local {
		return upvalue
	}

	createdUpvalue := newUpvalue(local)
	createdUpvalue.Next = upvalue

	if prevUpvalue == nil {
		vm.openUpvalues = createdUpvalue
	} else {
		prevUpvalue.Next = createdUpvalue
	}

	return createdUpvalue
}

func (vm *VM) newNative(name string, fn NativeFunction) {
	vm.globals[name] = nativeFunctionValue(fn)
}

func (vm *VM) callValue(callee Value, argCount uint) bool {
	switch callee.Type {
	case ValueClosure:
		return vm.call(callee.Closure(), argCount)
	case ValueNativeFunction:
		values := vm.stack[vm.stackTop-argCount:]
		result := callee.NativeFunction()(values)
		vm.stackTop -= argCount + 1
		vm.push(result)
		return true
	}

	vm.runtimeError("Can only call functions and classes.")
	return false
}

func (vm *VM) call(closure *Closure, argCount uint) bool {
	if argCount != closure.Function.Arity {
		vm.runtimeError("Expected %d arguments but got %d.", closure.Function.Arity, argCount)
		return false
	}

	if len(vm.frames) >= FrameMax {
		vm.runtimeError("Stack overflow.")
		return false
	}

	vm.frames = append(vm.frames, &CallFrame{
		closure: closure,
		ip:      0,
		slot:    vm.stackTop - argCount - 1,
	})
	return true
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
	frame := vm.currentFrame()
	op := frame.closure.Function.Chunk.codes[frame.ip]
	frame.ip += 1
	return op
}

func (vm *VM) readShort() uint16 {
	frame := vm.currentFrame()
	frame.ip += 2
	chunk := vm.currentFrame().closure.Function.Chunk
	return uint16(chunk.codes[frame.ip-2]<<8 | chunk.codes[frame.ip-1])
}

func (vm *VM) readConstant() Value {
	ci := vm.readByte()
	return vm.currentFrame().closure.Function.Chunk.constants[ci]
}

func (vm *VM) readString() string {
	return vm.readConstant().String()
}

func (vm *VM) currentFrame() *CallFrame {
	return vm.frames[len(vm.frames)-1]
}

func (vm *VM) currentClosure() *Closure {
	return vm.frames[len(vm.frames)-1].closure
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

	frame := vm.currentFrame()
	line := frame.closure.Function.Chunk.lines[frame.ip-1]
	fmt.Fprintf(os.Stderr, "[line %d] in script\n", line)

	for i := len(vm.frames) - 1; i >= 0; i-- {
		frame := vm.frames[i]
		fn := frame.closure.Function
		instruction := frame.ip - 1
		fmt.Fprintf(os.Stderr, "[line %d] in ", fn.Chunk.lines[instruction])
		if fn.Name == "" {
			fmt.Fprintf(os.Stderr, "script\n")
		} else {
			fmt.Fprintf(os.Stderr, "%s()\n", fn.Name)
		}
	}

	vm.resetStack()
}

type CallFrame struct {
	closure *Closure
	ip      uint
	slot    uint
}

func clockNative(args []Value) Value {
	return numberValue(float64(time.Now().Unix()))
}
