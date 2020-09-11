package main

import (
	"bufio"
	"context"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/gorilla/handlers"
	"github.com/gorilla/mux"
	"github.com/rs/zerolog"

	"github.com/EmilGedda/turplanering/srv/internal/api"
	"github.com/EmilGedda/turplanering/srv/internal/auth"
	"github.com/EmilGedda/turplanering/srv/internal/env"
	"github.com/EmilGedda/turplanering/srv/internal/net"
)

func main() {
	zerolog.TimeFieldFormat = zerolog.TimeFormatUnixMicro

	var out io.Writer = os.Stdout

	envVars := env.Vars()

	// hide behind config interface
	if envVars.Env == env.Development {
		out = zerolog.ConsoleWriter{
			Out:        os.Stdout,
			TimeFormat: "15:04:03.000",
		}
	}

	logger := zerolog.New(out).With().Timestamp().Logger()
	logger.Level(zerolog.DebugLevel)

	logger.Info().
		Str("env", string(envVars.Env)).
		Msg("Initializing Turplanering server")

	creds := envVars.Lantmateriet
	routes := []*api.Endpoint{}

	lantmateriet, err := auth.NewLantmateriet(
		auth.WithConsumerID(creds.ConsumerID),
		auth.WithConsumerKey(creds.ConsumerKey),
	)

	tokenRoutes := api.TokenAPIRoutes(lantmateriet)

	if err != nil {
		api.MarkUnavailable(tokenRoutes, err)
		logger.Warn().
			Err(err).
			Msg("Lantmäteriet not available")
	}

	routes = append(routes, tokenRoutes...)

	r := mux.NewRouter()

	r.Use(
		handlers.RecoveryHandler(
			handlers.PrintRecoveryStack(envVars.Env == env.Development),
		),
		handlers.CompressHandler,
		handlers.CORS(
			handlers.AllowedOrigins([]string{"https://emilgedda.github.io"}),
		),
		net.InjectResponseHeader(map[string][]string{
			"Content-Security-Policy": {"none"},
			"Content-Type":            {"application/json"},
			"X-Content-Type-Options":  {"nosniff"},
		}),
		net.InjectLogger(&logger),
		net.InjectRequestID,
		net.LogRequest,
	)

	srv := &http.Server{
		Addr:         "localhost:8080",
		WriteTimeout: time.Second * 15,
		ReadTimeout:  time.Second * 15,
		IdleTimeout:  time.Second * 60,
		Handler:      r,
	}

	for _, v := range routes {
		logger.Debug().
			Str("route", v.Route).
			Strs("methods", v.Methods).
			Str("handler", v.Name).
			Msg("Registered route")

		r.HandleFunc(v.Route, v.Handler).
			Name(v.Name).
			Methods(v.Methods...)
	}

	logger.Info().
		Str("addr", srv.Addr).
		Msg("Starting Turplanering server")

	c := make(chan os.Signal, 8)
	defer close(c)

	signal.Notify(c,
		syscall.SIGHUP,
		syscall.SIGINT,
		syscall.SIGTERM,
		syscall.SIGQUIT)

	logger.Info().
		Msg("Listening for connections...")

	// Start server
	go func() {
		if err := srv.ListenAndServe(); err != http.ErrServerClosed {
			logger.Error().Err(err).Msg("Server failed")
			c <- syscall.SIGTERM
		}
	}()

	// Closing stdin closes server
	go func() {
		reader := bufio.NewReader(os.Stdin)
		ioutil.ReadAll(reader)
		logger.Info().Msg("Stdin closed")
		c <- syscall.SIGTERM
	}()

	// Block until we receive our signal.
	sig := <-c

	lookup := map[os.Signal]string{
		syscall.SIGHUP:  "SIGHUP",
		syscall.SIGINT:  "SIGINT",
		syscall.SIGTERM: "SIGTERM",
		syscall.SIGQUIT: "SIGQUIT",
	}

	logger.Info().
		Str("signal", lookup[sig]).
		Msg("Received signal")

	logger.Info().
		Msg("Shutting down server...")

	before := time.Now()
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	_ = srv.Shutdown(ctx)
	after := time.Now()

	logger.Info().
		Dur("took", after.Sub(before)).
		Msg("Server down")
}
