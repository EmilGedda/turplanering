package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/gorilla/handlers"
	"github.com/gorilla/mux"

	"github.com/EmilGedda/turplanering/srv/internal/api"
	"github.com/EmilGedda/turplanering/srv/internal/auth"
)

func main() {
	tokenService := auth.NewLantmateriet()
	r := mux.NewRouter()

	routes := api.NewAPI(
		api.WithTokenService(tokenService),
	)

	r.Use(
		handlers.RecoveryHandler(),
		handlers.CompressHandler,
		handlers.CORS(
			handlers.AllowedOrigins([]string{"*"}),
		),
	)

	r.HandleFunc("/token", routes.GetTokenHandler).Methods(http.MethodGet)
	r.HandleFunc("/token", routes.RevokeTokenHandler).Methods(http.MethodDelete)
	r.HandleFunc("/token", routes.RefreshTokenHandler).Methods(http.MethodPut)

	srv := &http.Server{
		Addr:         "localhost:8080",
		WriteTimeout: time.Second * 15,
		ReadTimeout:  time.Second * 15,
		IdleTimeout:  time.Second * 60,
		Handler:      r,
	}

	// Run our server in a goroutine so that it doesn't block.
	go func() {
		if err := srv.ListenAndServe(); err != nil {
			log.Println(err)
		}
	}()

	c := make(chan os.Signal, 1)
	signal.Notify(c,
		syscall.SIGHUP,
		syscall.SIGINT,
		syscall.SIGTERM,
		syscall.SIGQUIT)

	// Block until we receive our signal.
	<-c

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	_ = srv.Shutdown(ctx)
}
