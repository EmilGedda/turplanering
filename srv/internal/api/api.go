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

type TokenAPI struct {
	auth.TokenService
}

func TokenAPIRoutes(ts auth.TokenService) []*Endpoint {
	newRoute := func(methods string, name string, handler http.HandlerFunc) *Endpoint {
		return &Endpoint{
			Route:   "/token",
			Methods: []string{methods},
			Name:    "(*API)." + name + "TokenHandler",
			Handler: handler,
		}
	}

	api := TokenAPI{ts}

	return []*Endpoint{
		newRoute(http.MethodGet, "Get", api.GetTokenHandler),
		newRoute(http.MethodPut, "Refresh", api.RefreshTokenHandler),
		newRoute(http.MethodDelete, "Revoke", api.RevokeTokenHandler),
	}
}

func MarkUnavailable(eps []*Endpoint, err error) {
	for _, ep := range eps {
		ep.Name = "ServiceUnvailableHandler"
		ep.Handler = ServiceUnvailableHandler(err)
	}
}
