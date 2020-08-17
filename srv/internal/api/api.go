package api

import "github.com/EmilGedda/turplanering/srv/internal/auth"

type API struct {
	ts auth.TokenService
}

type APIOption = func(*API)

func WithTokenService(ts auth.TokenService) APIOption {
	return func(a *API) { a.ts = ts }
}

func NewAPI(opts ...APIOption) *API {
	api := &API{}

	for _, opt := range opts {
		opt(api)
	}

	return api
}
