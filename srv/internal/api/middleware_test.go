package api

import (
	"context"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"github.com/felixge/httpsnoop"
	"github.com/rs/zerolog"
	"github.com/stretchr/testify/assert"
)

type mockHandler struct {
	ctx context.Context
}

func (m *mockHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	m.ctx = r.Context()
}

func newRequest() *http.Request {
	return httptest.NewRequest("GET", "/", nil)
}

func newRequestWithLogger() (*http.Request, *strings.Builder) {
	r := newRequest()
	sb := &strings.Builder{}
	logger := zerolog.New(sb)
	return r.WithContext(logger.WithContext(r.Context())), sb
}

func TestInjectLogger(t *testing.T) {
	mock := &mockHandler{}
	r := newRequest()
	logger := zerolog.New(ioutil.Discard).Level(zerolog.Disabled)
	InjectLogger(&logger)(mock).ServeHTTP(nil, r)
	assert.Equal(t, &logger, zerolog.Ctx(r.Context()))
}

func TestInjectRequestID(t *testing.T) {
	mock := &mockHandler{}
	r, out := newRequestWithLogger()
	InjectRequestID(32)(mock).ServeHTTP(nil, r)
	id := RequestIDFromContext(mock.ctx)
	assert.Contains(t, out.String(), id)
	assert.Len(t, id, 32)
	assert.Contains(t, out.String(), `"requestID":"`+id+`"`)
	assert.Contains(t, out.String(), "Injecting request ID")
	assert.Equal(t, id, r.Header.Get("X-Request-ID"))
}

func TestLogRequest(t *testing.T) {
	mock := &mockHandler{}
	r, out := newRequestWithLogger()
	LogRequest(mock).ServeHTTP(nil, r)
	assert.Contains(t, out.String(), "method")
	assert.Contains(t, out.String(), "url")
	assert.Contains(t, out.String(), "remote")
	assert.Contains(t, out.String(), "Serving request")
}

type mockSnooper struct {
	metrics httpsnoop.Metrics
}

func (s *mockSnooper) CaptureMetrics(http.Handler, http.ResponseWriter, *http.Request) httpsnoop.Metrics {
	return s.metrics
}

func TestLogResponse(t *testing.T) {
	mock := &mockHandler{}
	r, out := newRequestWithLogger()
	snooper := &mockSnooper{
		httpsnoop.Metrics{
			Code:     1337,
			Written:  9876,
			Duration: time.Hour,
		},
	}

	LogResponse(snooper)(mock).ServeHTTP(nil, r)
	assert.Contains(t, out.String(), `"status":1337`)
	assert.Contains(t, out.String(), "size")
	assert.Contains(t, out.String(), "elapsed")
	assert.Contains(t, out.String(), "Serving response")
	assert.Contains(t, out.String(), "Slow response time")

	assert.NotPanics(t, func() {
		external := &HttpSnooper{}
		external.CaptureMetrics(&mockHandler{}, httptest.NewRecorder(), newRequest())
	})
}

func TestInjectResponseHeader(t *testing.T) {
	mock := &mockHandler{}
	w := httptest.NewRecorder()
	r := newRequest()

	headers := map[string][]string{
		"Foo": {"Bar", "Quux"},
		"Key": {"Value1", "Value2"},
	}

	InjectResponseHeaders(headers)(mock).ServeHTTP(w, r)

	for header, values := range headers {
		assert.Equal(t, values, w.Result().Header[header])
	}
}
