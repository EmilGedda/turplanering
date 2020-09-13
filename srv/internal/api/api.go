package api

import (
	"encoding/json"
	"net/http"

	"github.com/rs/zerolog"

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

func TokenAPIRoutes(ts auth.TokenService) []Endpoint {
	newRoute := func(methods string, name string, handler tokenHandler) Endpoint {
		return Endpoint{
			Route:   "/token",
			Methods: []string{methods},
			Name:    "(*TokenAPI)." + name + "Handler",
			Handler: wrapTokenHandler(name, handler),
		}
	}

	api := TokenAPI{ts}
	return []Endpoint{
		newRoute(http.MethodGet, "GetToken", api.GetTokenHandler),
		newRoute(http.MethodPut, "RefreshToken", api.RefreshTokenHandler),
		newRoute(http.MethodDelete, "RevokeToken", api.RevokeTokenHandler),
	}
}

func MarkUnavailable(eps []Endpoint, err error) {
	for i := range eps {
		eps[i].Name = "ServiceUnavailableHandler"
		eps[i].Handler = func(w http.ResponseWriter, r *http.Request) {
			res, code := handlerError(err)
			w.WriteHeader(code)
			_ = json.NewEncoder(w).Encode(res) // wont fail
		}
	}
}

func wrapTokenHandler(name string, handler tokenHandler) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		logger := zerolog.Ctx(r.Context()).With().Str("handler", name+"Token").Logger()
		res, err := handler()
		if err != nil {
			logger.Err(err).Msg(name + " failed")
			errMsg, code := handlerError(err)
			res = errMsg
			w.WriteHeader(code)
		}

		if res == nil {
			return
		}

		err = json.NewEncoder(w).Encode(res)
		if err != nil {
			logger.Err(err).Msg("Unable to write response body")
		}
	}
}
