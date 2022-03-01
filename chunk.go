package main

import "fmt"

type OpCode uint
type Value float64

const (
	OpReturn OpCode = iota
	OpConstant
	OpNegate
	OpAdd
	OpSubtract
	OpMultiply
	OpDivide
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
	case OpAdd:
		return c.simpleInstruction("OP_ADD", offset)
	case OpSubtract:
		return c.simpleInstruction("OP_SUBTRACT", offset)
	case OpMultiply:
		return c.simpleInstruction("OP_MULTIPLY", offset)
	case OpDivide:
		return c.simpleInstruction("OP_DIVIDE", offset)
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
	printValue(c.constants[ci])
	fmt.Printf("'\n")

	return offset + 2
}

func printValue(value Value) {
	fmt.Printf("%g", value)
}
