package api

import (
	"encoding/json"
	"net/http"

	"github.com/pkg/errors"
	"github.com/rs/zerolog"
)

func ServiceUnvailableHandler(err error) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		logger := zerolog.Ctx(r.Context()).With().Str("handler", "service unavailable").Logger()
		handlerError(w, err, http.StatusServiceUnavailable, &logger)
	}
}

func (a *API) RefreshTokenHandler(w http.ResponseWriter, r *http.Request) {
	logger := zerolog.Ctx(r.Context()).With().Str("handler", "refresh token").Logger()
	token, err := a.ts.GetToken()
	if err != nil {
		logger.Err(err).Msg("Get token in refresh handler failed")
		handlerError(w, errors.Wrap(err, "get token"), http.StatusInternalServerError, &logger)
		return
	}

	token, err = a.ts.RefreshToken(token)
	if err != nil {
		logger.Err(err).Msg("Refresh token failed")
		handlerError(w, errors.Wrap(err, "refresh token"), http.StatusInternalServerError, &logger)
		return
	}

	err = json.NewEncoder(w).Encode(token)
	if err != nil {
		logger.Err(err).Msg("Unable to write token to response body")
		handlerError(w, errors.Wrap(err, "json encode"), http.StatusInternalServerError, &logger)
	}
}

func (a *API) RevokeTokenHandler(w http.ResponseWriter, r *http.Request) {
	logger := zerolog.Ctx(r.Context()).With().Str("handler", "revoke token").Logger()
	token, err := a.ts.GetToken()
	if err != nil {
		logger.Err(err).Msg("Get token failed")
		handlerError(w, errors.Wrap(err, "get token"), http.StatusInternalServerError, &logger)
		return
	}

	err = a.ts.RevokeToken(token)
	if err != nil {
		logger.Err(err).Msg("Revoke token failed")
		handlerError(w, errors.Wrap(err, "revoke token"), http.StatusInternalServerError, &logger)
	}

	w.WriteHeader(http.StatusOK)
}

func (a *API) GetTokenHandler(w http.ResponseWriter, r *http.Request) {
	logger := zerolog.Ctx(r.Context()).With().Str("handler", "get token").Logger()
	token, err := a.ts.GetToken()
	if err != nil {
		logger.Err(err).Msg("Get token failed")
		handlerError(w, errors.Wrap(err, "get token"), http.StatusInternalServerError, &logger)
		return
	}

	err = json.NewEncoder(w).Encode(token)
	if err != nil {
		logger.Err(err).Msg("Unable to write token to response body")
		handlerError(w, err, http.StatusInternalServerError, &logger)
	}
}

func handlerError(w http.ResponseWriter, err error, statusCode int, logger *zerolog.Logger) {
	type response struct {
		StatusText string `json:"status_text"`
		StatusCode int    `json:"status_code"`
		Error      error  `json:"error"`
		ErrorStr   string `json:"error_string"`
	}

	res := response{
		StatusText: http.StatusText(statusCode),
		StatusCode: statusCode,
		Error:      err,
		ErrorStr:   err.Error(),
	}

	out := `{"status_text:"Internal Server Error","status_code":500,"error":"json marshal failure in handlerError"}`
	data, err := json.Marshal(res)
	if err != nil {
		logger.Err(err).
			Interface("response", res).
			Msg("Json marshal failure in handlerError")
	} else {
		out = string(data)
	}

	http.Error(w, out, statusCode)
}
