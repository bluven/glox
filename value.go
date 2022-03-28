package main

import "fmt"

type ValueType int
type ValueFn func(v interface{}) Value

const (
	ValueNil ValueType = iota
	ValueBool
	ValueNumber
	ValueObject
	ValueString
	ValueFunction
	ValueNativeFunction
	ValueClosure
)

type Value struct {
	Type ValueType
	Raw  interface{}
}

func nilValue() Value {
	return Value{
		Type: ValueNil,
		Raw:  nil,
	}
}

func boolValue(v interface{}) Value {
	return Value{
		Type: ValueBool,
		Raw:  v,
	}
}

func numberValue(v interface{}) Value {
	return Value{
		Type: ValueNumber,
		Raw:  v,
	}
}

func objectValue(v interface{}) Value {
	return Value{Type: ValueObject, Raw: v}
}

func functionValue(v *Function) Value {
	return Value{Type: ValueFunction, Raw: v}
}

func nativeFunctionValue(v interface{}) Value {
	return Value{Type: ValueNativeFunction, Raw: v}
}

func stringValue(v interface{}) Value {
	return Value{Type: ValueString, Raw: v}
}

func closureValue(v *Closure) Value {
	return Value{Type: ValueClosure, Raw: v}
}

func (v Value) IsFalsey() bool {
	return v.IsNil() || (v.IsBool() && !v.Bool())
}

func (v Value) IsNil() bool {
	return v.Type == ValueNil
}

func (v Value) IsBool() bool {
	return v.Type == ValueBool
}

func (v Value) IsNumber() bool {
	return v.Type == ValueNumber
}

func (v Value) IsObject() bool {
	return v.Type == ValueObject
}

func (v Value) IsFunction() bool {
	return v.Type == ValueFunction
}

func (v Value) IsNativeFunction() bool {
	return v.Type == ValueNativeFunction
}

func (v Value) IsClosure() bool {
	return v.Type == ValueClosure
}

func (v Value) IsString() bool {
	return v.Type == ValueString
}

func (v Value) IsObjectType(t ObjectType) bool {
	return v.IsObject() && v.Raw.(*Object).Type == t
}

func (v Value) Number() float64 {
	return v.Raw.(float64)
}

func (v Value) Bool() bool {
	return v.Raw.(bool)
}

func (v Value) Object() *Object {
	return v.Raw.(*Object)
}

func (v Value) String() string {
	return v.Raw.(string)
}

func (v Value) Function() *Function {
	return v.Raw.(*Function)
}

func (v Value) Closure() *Closure {
	return v.Raw.(*Closure)
}

func (v Value) NativeFunction() NativeFunction {
	return v.Raw.(NativeFunction)
}

func (v Value) Print() {
	switch v.Type {
	case ValueBool:
		fmt.Printf("%t", v.Bool())
	case ValueNil:
		fmt.Print("nil")
	case ValueNumber:
		fmt.Printf("%g", v.Number())
	case ValueString:
		fmt.Printf("%s", v.Raw)
	case ValueFunction:
		fmt.Printf("%s", v.Raw.(*Function).Name)
	case ValueObject:
		panic("not implemented")
	}
}

func (v Value) Equal(o Value) bool {
	if v.Type != o.Type {
		return false
	}

	switch v.Type {
	case ValueNil:
		return true
	case ValueBool:
		return v.Bool() == o.Bool()
	case ValueNumber:
		return v.Number() == o.Number()
	case ValueString:
		return v.Raw == o.Raw
	default:
		return false
	}
}
