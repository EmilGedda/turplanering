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
	return "json to marshal: " + string(e.Json) + ": " + e.Err.Error()
}

type NotInitializedError struct {
	Module string
	Needs  []string
}

func (e *NotInitializedError) Error() string {
	last := len(e.Needs) - 1
	missing := e.Needs[last]
	if last > 0 {
		missing = strings.Join(e.Needs[:last], ", ") + " and " + e.Needs[last]
	}
	return e.Module + " is not initialized, missing " + missing
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
