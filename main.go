package main

func main() {
	chunk := NewChunk()
	chunk.Write(OpReturn)
	chunk.DisassembleChunk("chunk")
	chunk.Free()
}
