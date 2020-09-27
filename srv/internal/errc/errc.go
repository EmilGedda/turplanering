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
	nonNils := []error{}
	for _, err := range []error(*e) {
		if err != nil {
			nonNils = append(nonNils, err)
		}
	}

	sb := strings.Builder{}
	last := len(nonNils) - 1

	if last < 1 {
		return "no non nil errors"
	}

	sb.WriteString("{")
	for _, err := range nonNils[:last] {
		sb.WriteString(err.Error())
		sb.WriteString(", ")
	}

	sb.WriteString(nonNils[last].Error())
	sb.WriteString("}")
	return sb.String()
}

func (e *CompoundError) HasErrors() bool {
	for _, err := range []error(*e) {
		if err != nil {
			return true
		}
	}
	return false
}
