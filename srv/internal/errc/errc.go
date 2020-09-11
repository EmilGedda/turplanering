package errc

import "strings"

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
	missing := strings.Join(e.Needs[:last], ", ") + " and " + e.Needs[last]
	return e.Module + " is not initialized, missing " + missing
}
