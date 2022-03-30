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

type ObjectClass struct {
	Name    string
	Methods map[string]Value
}

func newClass(name string) *ObjectClass {
	return &ObjectClass{Name: name, Methods: make(map[string]Value)}
}

type ObjectInstance struct {
	Class  *ObjectClass
	Fields map[string]Value
}

func newInstance(clazz *ObjectClass) *ObjectInstance {
	return &ObjectInstance{
		Class:  clazz,
		Fields: make(map[string]Value),
	}
}

type BoundedMethod struct {
	Receiver Value
	Method   *Closure
}

func newBoundedMethod(value Value, method *Closure) *BoundedMethod {
	return &BoundedMethod{
		Receiver: value,
		Method:   method,
	}
}
