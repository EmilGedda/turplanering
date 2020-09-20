package errc

import (
	"encoding/json"
	"strings"
)

type UnmarshalError struct {
	Err  error
	Json []byte
	Obj  interface{}
}

func (e *UnmarshalError) Unwrap() error {
	return e.Err
}

func (e *UnmarshalError) Causer() error {
	return e.Err
}

func (e *UnmarshalError) Error() string {
	return "json unmarshal failed: " + string(e.Json) + ": " + e.Err.Error()
}

type NotInitializedError struct {
	Module string
	Needs  []string
}

func (e *NotInitializedError) Error() string {
	str := e.Module + " is not initialized"
	last := len(e.Needs) - 1
	if last < 0 {
		return str
	}
	missing := e.Needs[last]
	if last > 0 {
		missing = strings.Join(e.Needs[:last], ", ") + " and " + e.Needs[last]
	}
	return str + ", missing " + missing
}

type WrappedError struct {
	Err error
	Msg string
}

func Wrap(err error, msg string) *WrappedError {
	return &WrappedError{Err: err, Msg: msg}
}

func (e *WrappedError) Error() string {
	return e.Msg + ": " + e.Err.Error()
}

func (e *WrappedError) MarshalJSON() ([]byte, error) {
	return json.Marshal(e.Err)
}

func (e *WrappedError) Unwrap() error {
	return e.Err
}

func (e *WrappedError) Causer() error {
	return e.Err
}

type CompoundError []error

func (e *CompoundError) Error() string {
	errors := []error(*e)
	sb := strings.Builder{}
	last := len(errors) - 1

	for _, err := range errors[:last] {
		sb.WriteString(err.Error())
		sb.WriteString(", ")
	}

	sb.WriteString(errors[last].Error())
	return sb.String()
}
