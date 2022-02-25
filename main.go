package main

func main() {
	chunk := NewChunk()
	ci := chunk.AddConstant(1.2)
	chunk.Write(OpConstant, 123)
	chunk.Write(OpCode(ci), 123)
	chunk.Write(OpReturn, 123)
	chunk.DisassembleChunk("chunk")
	chunk.Free()
}
