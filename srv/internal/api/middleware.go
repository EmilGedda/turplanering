package api

import (
	"context"
	"math/rand"
	"net/http"
	"time"

	"github.com/felixge/httpsnoop"
	"github.com/rs/zerolog"
)

func InjectLogger(logger *zerolog.Logger) func(http.Handler) http.Handler {
	return func(h http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			h.ServeHTTP(w, r.WithContext(logger.WithContext(r.Context())))
		})
	}
}

func InjectRequestID(length int) func(http.Handler) http.Handler {
	return func(h http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			ctx := r.Context()
			logger := zerolog.Ctx(ctx)
			reqID := NewRequestID(length)
			newLogger := logger.With().Str("requestID", reqID).Logger()
			newLogger.Trace().Msg("Injecting request ID into logger")
			ctx = newLogger.WithContext(ctx)
			r.Header.Add("X-Request-ID", reqID)
			h.ServeHTTP(w, r.WithContext(ContextWithRequestID(ctx, reqID)))
		})
	}
}

func LogRequest(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		logger := zerolog.Ctx(r.Context())
		logger.Info().
			Str("method", r.Method).
			Str("url", r.URL.String()).
			Str("remote", r.RemoteAddr).
			Msg("Serving request")
		h.ServeHTTP(w, r)
	})
}

type Snooper interface {
	CaptureMetrics(hnd http.Handler, w http.ResponseWriter, r *http.Request) httpsnoop.Metrics
}

type HttpSnooper struct{}

func (s *HttpSnooper) CaptureMetrics(hnd http.Handler, w http.ResponseWriter, r *http.Request) httpsnoop.Metrics {
	return httpsnoop.CaptureMetrics(hnd, w, r)
}

func LogResponse(snooper Snooper) func(http.Handler) http.Handler {
	return func(h http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			logger := zerolog.Ctx(r.Context())
			metrics := snooper.CaptureMetrics(h, w, r)
			logger.Info().
				Int("status", metrics.Code).
				Int64("size", metrics.Written).
				Dur("elapsed", metrics.Duration).
				Msg("Serving response")

			if metrics.Duration > 500*time.Millisecond {
				logger.Warn().Msg("Slow response time")
			}
		})
	}
}

func InjectResponseHeaders(headers http.Header) func(http.Handler) http.Handler {
	return func(h http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			for header, values := range headers {
				w.Header().Del(header)
				for _, value := range values {
					w.Header().Add(header, value)
				}
			}
			h.ServeHTTP(w, r)
		})
	}
}

type requestIDCtxKey struct{}

var requestIDCtxValue requestIDCtxKey

func ContextWithRequestID(ctx context.Context, reqID string) context.Context {
	return context.WithValue(ctx, requestIDCtxValue, reqID)
}

func RequestIDFromContext(ctx context.Context) string {
	return ctx.Value(requestIDCtxValue).(string)
}

// NewRequestID requires n > 0.
func NewRequestID(n int) string {
	const (
		alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
		nbits    = 6
		mask     = 1<<nbits - 1
		max      = 63 / nbits
	)

	b := make([]byte, n)
	for i, cache, remain := n-1, rand.Int63(), max; i >= 0; {
		if remain == 0 {
			cache, remain = rand.Int63(), max
		}
		if idx := int(cache & mask); idx < len(alphabet) {
			b[i] = alphabet[idx]
			i--
		}
		cache >>= nbits
		remain--
	}
	return string(b)
}
