package errc

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
