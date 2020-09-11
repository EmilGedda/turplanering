package api

import (
	"net/http"

	"github.com/EmilGedda/turplanering/srv/internal/auth"
)

type Endpoint struct {
	Name    string
	Route   string
	Methods []string
	Handler http.HandlerFunc
}

type API struct {
	ts        auth.TokenService
	Endpoints []Endpoint
}

type APIOption = func(*API)

func WithTokenService(ts auth.TokenService, err error) APIOption {
	newRoute := func(methods string, name string, handler http.HandlerFunc) Endpoint {
		ep := Endpoint{
			Route:   "/token",
			Methods: []string{methods},
			Name:    "(*API)." + name + "TokenHandler",
			Handler: handler,
		}
		if err != nil {
			ep.Handler = ServiceUnvailableHandler(err)
		}
		return ep
	}

	return func(a *API) {
		a.Endpoints = append(a.Endpoints, []Endpoint{
			newRoute(http.MethodGet, "Get", a.GetTokenHandler),
			newRoute(http.MethodPut, "Refresh", a.RefreshTokenHandler),
			newRoute(http.MethodDelete, "Revoke", a.RevokeTokenHandler),
		}...)
	}
}

func NewAPI(opts ...APIOption) *API {
	api := &API{}

	for _, opt := range opts {
		opt(api)
	}

	return api
}
