package main

import "fmt"

type ValueType int
type ValueFn func(v interface{}) Value

const (
	ValueNil ValueType = iota
	ValueBool
	ValueNumber
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

func (v Value) Number() float64 {
	return v.Raw.(float64)
}

func (v Value) Bool() bool {
	return v.Raw.(bool)
}

func (v Value) Print() {
	switch v.Type {
	case ValueBool:
		fmt.Printf("%t", v.Bool())
	case ValueNil:
		fmt.Print("nil")
	case ValueNumber:
		fmt.Printf("%g", v.Number())
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
	default:
		return false
	}
}
