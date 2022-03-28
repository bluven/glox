package main

import "fmt"

type OpCode = uint

const (
	OpReturn OpCode = iota
	OpConstant
	OpNegate
	OpPrint
	OpNil
	OpTrue
	OpFalse
	OpPop
	OpCloseUpvalue
	OpEqual
	OpGreater
	OpLess
	OpAdd
	OpSubtract
	OpMultiply
	OpDivide
	OpNot
	OpDefineGlobal
	OpGetGlobal
	OpSetGlobal
	OpGetLocal
	OpSetLocal
	OpGetUpValue
	OpSetUpValue
	OpJump
	OpJumpIfFalse
	OpLoop
	OpCall
	OpClosure
)

type Chunk struct {
	codes     []OpCode
	constants []Value
	lines     []int
}

func NewChunk() *Chunk {
	c := &Chunk{}
	c.init()
	return c
}

func (c *Chunk) init() {
	c.codes = nil
	c.lines = nil
	c.constants = nil
}

func (c *Chunk) Write(op OpCode, line int) {
	c.codes = append(c.codes, op)
	c.lines = append(c.lines, line)
}

func (c *Chunk) AddConstant(value Value) int {
	c.constants = append(c.constants, value)
	return len(c.constants) - 1
}

func (c *Chunk) Free() {
	c.init()
}

func (c *Chunk) count() int {
	return len(c.codes)
}

func (c *Chunk) DisassembleChunk(name string) {
	fmt.Printf("== %s ==\n", name)
	for offset := 0; offset < len(c.codes); {
		offset = c.disassembleInstruction(offset)
	}
}

func (c *Chunk) disassembleInstruction(offset int) int {
	fmt.Printf("%04d ", offset)
	if offset > 0 && c.lines[offset] == c.lines[offset-1] {
		fmt.Printf("   | ")
	} else {
		fmt.Printf("%4d ", c.lines[offset])
	}

	op := c.codes[offset]
	switch op {
	case OpReturn:
		return c.simpleInstruction("OP_RETURN", offset)
	case OpConstant:
		return c.constantInstruction("OP_CONSTANT", offset)
	case OpNegate:
		return c.simpleInstruction("OP_NEGATE", offset)
	case OpEqual:
		return c.simpleInstruction("OP_EQUAL", offset)
	case OpGreater:
		return c.simpleInstruction("OP_GREATER", offset)
	case OpLess:
		return c.simpleInstruction("OP_LESS", offset)
	case OpAdd:
		return c.simpleInstruction("OP_ADD", offset)
	case OpSubtract:
		return c.simpleInstruction("OP_SUBTRACT", offset)
	case OpMultiply:
		return c.simpleInstruction("OP_MULTIPLY", offset)
	case OpDivide:
		return c.simpleInstruction("OP_DIVIDE", offset)
	case OpNil:
		return c.simpleInstruction("OP_NIL", offset)
	case OpTrue:
		return c.simpleInstruction("OP_TRUE", offset)
	case OpFalse:
		return c.simpleInstruction("OP_FALSE", offset)
	case OpNot:
		return c.simpleInstruction("OP_NOT", offset)
	case OpPrint:
		return c.simpleInstruction("OP_PRINT", offset)
	case OpPop:
		return c.simpleInstruction("OP_POP", offset)
	case OpCloseUpvalue:
		return c.simpleInstruction("OP_CLOSE_UPVALUE", offset)
	case OpDefineGlobal:
		return c.constantInstruction("OP_DEFINE_GLOBAL", offset)
	case OpGetGlobal:
		return c.constantInstruction("OP_GET_GLOBAL", offset)
	case OpSetGlobal:
		return c.constantInstruction("OP_SET_GLOBAL", offset)
	case OpGetLocal:
		return c.byteInstruction("OP_GET_LOCAL", offset)
	case OpSetLocal:
		return c.byteInstruction("OP_SET_LOCAL", offset)
	case OpGetUpValue:
		return c.byteInstruction("OP_GET_UPVALUE", offset)
	case OpSetUpValue:
		return c.byteInstruction("OP_SET_UPVALUE", offset)
	case OpJump:
		return c.jumpInstruction("OP_JUMP", 1, offset)
	case OpJumpIfFalse:
		return c.jumpInstruction("OP_JUMP_IF_FALSE", 1, offset)
	case OpLoop:
		return c.jumpInstruction("OP_LOOP", -1, offset)
	case OpCall:
		return c.byteInstruction("OP_CALL", offset)
	case OpClosure:
		constant := c.codes[offset+1]
		fmt.Printf("%-16s %4d ", "OP_CLOSURE", constant)
		c.constants[constant].Print()
		fmt.Println()
		offset += 2

		function := c.constants[constant].Function()
		for range function.Upvalues {
			t := "local"
			if c.codes[offset] == 0 {
				t = "upvalue"
			}
			index := c.codes[offset+1]
			fmt.Printf("%04d      |                     %s %d\n", offset, t, index)
			offset += 2
		}

		return offset
	default:
		fmt.Printf("Unknown opcode %d\n", op)
		return offset + 1
	}
}

func (c *Chunk) simpleInstruction(name string, offset int) int {
	fmt.Printf("%s\n", name)
	return offset + 1
}

func (c *Chunk) constantInstruction(name string, offset int) int {
	ci := c.codes[offset+1]
	fmt.Printf("%-16s %4d '", name, ci)
	c.constants[ci].Print()
	fmt.Printf("'\n")

	return offset + 2
}

func (c *Chunk) byteInstruction(name string, offset int) int {
	slot := c.codes[offset+1]
	fmt.Printf("%-16s %4d\n", name, slot)
	return offset + 2
}

func (c *Chunk) jumpInstruction(name string, sign int, offset int) int {
	jump := c.codes[offset+1] << 8
	jump |= c.codes[offset+2]
	fmt.Printf("%-16s %4d -> %d\n", name, offset, offset+3+sign*int(jump))
	return offset + 3
}
