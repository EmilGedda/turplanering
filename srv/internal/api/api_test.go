package api

import (
	"errors"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/rs/zerolog"
	"github.com/stretchr/testify/assert"

	"github.com/EmilGedda/turplanering/srv/internal/auth"
)

func TestTokenAPIRoutes(t *testing.T) {
	validMethod := func(methods []string) bool {
		for _, method := range methods {
			if method != "GET" && method != "PUT" && method != "DELETE" {
				return false
			}
		}
		return true
	}

	routes := TokenAPIRoutes(&mockTokenService{})
	for _, route := range routes {
		assert.True(t, strings.HasSuffix(route.Name, "TokenHandler"))
		assert.True(t, validMethod(route.Methods))
		assert.Equal(t, "/token", route.Route)
	}
}

func TestMarkUnavailable(t *testing.T) {
	routes := []Endpoint{{Name: "Name"}}
	type wants struct {
		code        int
		errorString string
	}
	tests := []struct {
		have error
		want wants
	}{
		{nil, wants{500, "unknown error"}},
		{errors.New("foo error"), wants{500, "foo error"}},
		{auth.NewTokenErrResponse("bar error", ""), wants{503, "bar error"}},
	}

	for _, test := range tests {
		MarkUnavailable(routes, test.have)
		for _, route := range routes {
			assert.Equal(t, "ServiceUnavailableHandler", route.Name)

			w := httptest.NewRecorder()
			r := httptest.NewRequest("GET", "/token", nil)
			route.Handler(w, r)
			res := w.Result()
			body, _ := ioutil.ReadAll(res.Body)

			assert.Equal(t, test.want.code, res.StatusCode)
			assert.Contains(t, string(body), test.want.errorString)
		}
	}
}

func TestWrapTokenHandler(t *testing.T) {
	var out strings.Builder
	logger := zerolog.New(&out).With().Logger()
	json := map[string]string{
		"key": "test value",
	}
	err := errors.New("foo error")

	handler := wrapTokenHandler("Wrapped", func() (interface{}, error) {
		return json, err
	})

	w := httptest.NewRecorder()
	r := httptest.NewRequest("GET", "/token", nil)
	ctx := logger.WithContext(r.Context())
	r = r.WithContext(ctx)

	handler(w, r)

	res := w.Result()
	body, _ := ioutil.ReadAll(res.Body)

	assert.Equal(t, 500, res.StatusCode)
	assert.Contains(t, string(body), "foo error")
	assert.Contains(t, out.String(), "foo error")
	assert.Contains(t, out.String(), "Wrapped failed")
	assert.Contains(t, out.String(), "WrappedHandler")

	err = nil
	w = httptest.NewRecorder()

	handler(w, r)

	res = w.Result()
	body, _ = ioutil.ReadAll(res.Body)
	assert.Contains(t, string(body), "test value")
	assert.Equal(t, 200, res.StatusCode)

	handler = wrapTokenHandler("Wrapped", func() (interface{}, error) {
		return struct {
			Msg string `json:"msg"`
		}{"response"}, nil
	})

	out.Reset()
	handler(&writer{}, r)
	assert.Contains(t, out.String(), "Unable to write response body")

	handler = wrapTokenHandler("Wrapped", func() (interface{}, error) {
		return nil, nil
	})

	out.Reset()
	handler(&writer{}, r)
	assert.NotContains(t, out.String(), "Unable to write response body")
}

type writer struct{}

func (w *writer) Write([]byte) (int, error) {
	return -1, errors.New("write error")
}

func (w *writer) WriteHeader(int) {}

func (w *writer) Header() http.Header {
	return map[string][]string{}
}
