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
