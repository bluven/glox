package main

import "fmt"

type OpCode uint

const (
	OpReturn OpCode = iota
)

type Chunk struct {
	codes []OpCode
}

func NewChunk() *Chunk {
	c := &Chunk{}
	c.init()
	return c
}

func (c *Chunk) init() {
	c.codes = nil
}

func (c *Chunk) Write(op OpCode) {
	c.codes = append(c.codes, op)
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

	op := c.codes[offset]
	switch op {
	case OpReturn:
		return simpleInstruction("OP_RETURN", offset)
	default:
		fmt.Printf("Unknown opcode %d\n", op)
		return offset + 1
	}
}

func simpleInstruction(name string, offset int) int {
	fmt.Printf("%s\n", name)
	return offset + 1
}
