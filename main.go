package main

import "flag"

var (
	disassemble    = flag.Bool("disassemble", false, "")
	traceExecution = flag.Bool("trace-execution", false, "")
)

func main() {
	flag.Parse()

	vm := NewVM(*traceExecution)
	chunk := NewChunk()

	ci := chunk.AddConstant(1.2)
	chunk.Write(OpConstant, 123)
	chunk.Write(OpCode(ci), 123)

	ci = chunk.AddConstant(3.4)
	chunk.Write(OpConstant, 123)
	chunk.Write(OpCode(ci), 123)

	chunk.Write(OpAdd, 123)

	ci = chunk.AddConstant(5.6)
	chunk.Write(OpConstant, 123)
	chunk.Write(OpCode(ci), 123)

	chunk.Write(OpDivide, 123)

	chunk.Write(OpNegate, 123)
	chunk.Write(OpReturn, 123)

	vm.Interpret(chunk)
	vm.Free()

	if *disassemble {
		chunk.DisassembleChunk("chunk")
	}

	chunk.Free()
}
