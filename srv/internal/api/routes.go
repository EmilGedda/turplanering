package api

import (
	"errors"
	"net/http"

	"github.com/EmilGedda/turplanering/srv/internal/auth"
	"github.com/EmilGedda/turplanering/srv/internal/errc"
)

type tokenHandler = func() (interface{}, error)

func (service *TokenAPI) GetTokenHandler() (interface{}, error) {
	token, err := service.GetToken()
	if err != nil {
		return nil, errc.Wrap(err, "get token")
	}

	return token, nil
}

func (service *TokenAPI) RefreshTokenHandler() (interface{}, error) {
	currentToken, err := service.GetToken()
	if err != nil {
		return nil, errc.Wrap(err, "get token")
	}

	newToken, err := service.RefreshToken(currentToken)
	if err != nil {
		return nil, errc.Wrap(err, "refresh token")
	}

	return newToken, nil
}

func (service *TokenAPI) RevokeTokenHandler() (interface{}, error) {
	token, err := service.GetToken()
	if err != nil {
		return nil, errc.Wrap(err, "get token")
	}

	err = service.RevokeToken(token)
	if err != nil {
		return nil, errc.Wrap(err, "revoke token")
	}

	return struct {
		Message string `json:"message"`
	}{"revocation successful"}, nil
}

func handlerError(err error) (interface{}, int) {
	var (
		code       = http.StatusInternalServerError
		apiErr     *auth.TokenErrResponse
		notInitErr *errc.NotInitializedError
	)

	if errors.As(err, &apiErr) || errors.As(err, &notInitErr) {
		code = http.StatusServiceUnavailable
	}

	if err == nil {
		err = errors.New("unknown error")
	}

	return struct {
		StatusText string `json:"status_text"`
		StatusCode int    `json:"status_code"`
		Error      error  `json:"error"`
		ErrorStr   string `json:"error_string"`
	}{
		StatusText: http.StatusText(code),
		StatusCode: code,
		Error:      err,
		ErrorStr:   err.Error(),
	}, code
}
