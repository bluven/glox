package main

type ObjectType int

const (
	ObjectString ObjectType = iota
)

type Object struct {
	Type ObjectType
	Data interface{}
	Next *Object
}

func (obj *Object) Equal(obj2 *Object) bool {
	if obj.Type != obj2.Type {
		return false
	}

	return obj.Data == obj.Data
}

type Function struct {
	Arity uint
	Chunk *Chunk
	Name  string
}

func newFunction(name string) *Function {
	return &Function{
		Name:  name,
		Arity: 0,
		Chunk: NewChunk(),
	}
}
