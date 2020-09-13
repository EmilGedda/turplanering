package api

import (
	"encoding/json"
	"errors"
	"net/http"

	"github.com/rs/zerolog"

	"github.com/EmilGedda/turplanering/srv/internal/auth"
	"github.com/EmilGedda/turplanering/srv/internal/errc"
)

func ServiceUnvailableHandler(err error) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		logger := zerolog.Ctx(r.Context()).With().Str("handler", "service unavailable").Logger()
		handlerError(w, err, &logger)
	}
}

func (service *TokenAPI) GetTokenHandler(w http.ResponseWriter, r *http.Request) {
	logger := zerolog.Ctx(r.Context()).With().Str("handler", "get token").Logger()
	token, err := service.GetToken()
	if err != nil {
		logger.Err(err).Msg("Get token failed")
		handlerError(w, errc.Wrap(err, "get token"), &logger)
		return
	}

	err = json.NewEncoder(w).Encode(token)
	if err != nil {
		logger.Err(err).Msg("Unable to write token to response body")
		handlerError(w, err, &logger)
	}
}

func (service *TokenAPI) RefreshTokenHandler(w http.ResponseWriter, r *http.Request) {
	logger := zerolog.Ctx(r.Context()).With().Str("handler", "refresh token").Logger()
	token, err := service.GetToken()
	if err != nil {
		logger.Err(err).Msg("Get token in refresh handler failed")
		handlerError(w, errc.Wrap(err, "get token"), &logger)
		return
	}

	token, err = service.RefreshToken(token)
	if err != nil {
		logger.Err(err).Msg("Refresh token failed")
		handlerError(w, errc.Wrap(err, "refresh token"), &logger)
		return
	}

	err = json.NewEncoder(w).Encode(token)
	if err != nil {
		logger.Err(err).Msg("Unable to write token to response body")
		handlerError(w, errc.Wrap(err, "json encode"), &logger)
	}
}

func (service *TokenAPI) RevokeTokenHandler(w http.ResponseWriter, r *http.Request) {
	logger := zerolog.Ctx(r.Context()).With().Str("handler", "revoke token").Logger()
	token, err := service.GetToken()
	if err != nil {
		logger.Err(err).Msg("Get token failed")
		handlerError(w, errc.Wrap(err, "get token"), &logger)
		return
	}

	err = service.RevokeToken(token)
	if err != nil {
		logger.Err(err).Msg("Revoke token failed")
		handlerError(w, errc.Wrap(err, "revoke token"), &logger)
	}

	w.WriteHeader(http.StatusOK)
}

func handlerError(w http.ResponseWriter, err error, logger *zerolog.Logger) {
	type response struct {
		StatusText string `json:"status_text"`
		StatusCode int    `json:"status_code"`
		Error      error  `json:"error"`
		ErrorStr   string `json:"error_string"`
	}

	code := http.StatusInternalServerError
	var apiErr *auth.TokenErrResponse
	if errors.As(err, &apiErr) {
		code = http.StatusServiceUnavailable
	}

	res := response{
		StatusText: http.StatusText(code),
		StatusCode: code,
		Error:      err,
		ErrorStr:   err.Error(),
	}

	data, _ := json.Marshal(res) // cant error with given struct

	w.WriteHeader(code)
	n, err := w.Write(data)
	if err != nil {
		logger.Err(err).
			Int("count", n).
			Msg("Unable to write error to response body")
	}
}
