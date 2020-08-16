package net

import (
	"io"
	"net/http"
)

func ProxyHandler() func(http.ResponseWriter, *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		io.WriteString(w, "Hello, world!")
	}
}
