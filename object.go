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
	Arity    uint
	Chunk    *Chunk
	Name     string
	Upvalues []Upvalue
}

type Upvalue struct {
	Index   uint
	IsLocal bool
}

func newFunction(name string) *Function {
	return &Function{
		Name:  name,
		Arity: 0,
		Chunk: NewChunk(),
	}
}

type NativeFunction func(values []Value) Value

type Closure struct {
	Function *Function
	Upvalues []*RuntimeUpvalue
}

func newClosure(fn *Function) *Closure {
	return &Closure{
		Function: fn,
		Upvalues: make([]*RuntimeUpvalue, len(fn.Upvalues)),
	}
}

type RuntimeUpvalue struct {
	Location uint
	Next     *RuntimeUpvalue
	Value    Value
	Closed   bool
}

func newUpvalue(valueIndex uint) *RuntimeUpvalue {
	return &RuntimeUpvalue{Location: valueIndex}
}
