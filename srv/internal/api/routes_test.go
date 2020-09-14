package api

import (
	"encoding/json"
	"errors"
	"testing"
	"time"

	"github.com/EmilGedda/turplanering/srv/internal/auth"
	"github.com/stretchr/testify/assert"
)

type mockTokenService struct {
	token auth.Token
	gerr  error
	rerr  error
}

func (m *mockTokenService) GetToken() (*auth.Token, error) {
	return &m.token, m.gerr
}

func (m *mockTokenService) RevokeToken(*auth.Token) error {
	return m.rerr
}

func (m *mockTokenService) RefreshToken(*auth.Token) (*auth.Token, error) {
	return &m.token, m.rerr
}

var defaultMock = mockTokenService{
	auth.Token{
		AccessToken: "accesstoken",
		ExpiresIn:   3 * time.Second,
	},
	errors.New("foo"),
	errors.New("bar"),
}

func TestGetTokenHandler(t *testing.T) {
	mock := defaultMock
	api := &TokenAPI{&mock}
	res, err := api.GetTokenHandler()
	assert.Error(t, err)
	assert.Nil(t, res)
	assert.Contains(t, err.Error(), "get token")
	assert.Contains(t, err.Error(), "foo")

	mock.gerr = nil
	res, err = api.GetTokenHandler()
	assert.NoError(t, err)
	token, ok := res.(*auth.Token)
	assert.True(t, ok)
	assert.Equal(t, "accesstoken", token.AccessToken)
	assert.Equal(t, 3*time.Second, token.ExpiresIn)
}

func TestRefreshTokenHandler(t *testing.T) {
	mock := defaultMock
	api := &TokenAPI{&mock}
	res, err := api.RefreshTokenHandler()
	assert.Error(t, err)
	assert.Nil(t, res)
	assert.Contains(t, err.Error(), "get token")
	assert.NotContains(t, err.Error(), "refresh token")
	assert.Contains(t, err.Error(), "foo")

	mock.gerr = nil
	res, err = api.RefreshTokenHandler()
	assert.Error(t, err)
	assert.Nil(t, res)
	assert.Contains(t, err.Error(), "refresh token")
	assert.NotContains(t, err.Error(), "get token")
	assert.Contains(t, err.Error(), "bar")

	mock.rerr = nil
	res, err = api.RefreshTokenHandler()
	assert.NoError(t, err)
	token, ok := res.(*auth.Token)
	assert.True(t, ok)
	assert.Equal(t, "accesstoken", token.AccessToken)
	assert.Equal(t, 3*time.Second, token.ExpiresIn)
}

func TestRevokeTokenHandler(t *testing.T) {
	mock := defaultMock
	api := &TokenAPI{&mock}
	res, err := api.RevokeTokenHandler()
	assert.Error(t, err)
	assert.Nil(t, res)
	assert.Contains(t, err.Error(), "get token")
	assert.Contains(t, err.Error(), "foo")
	assert.NotContains(t, err.Error(), "revoke token")

	mock.gerr = nil
	res, err = api.RevokeTokenHandler()
	assert.Error(t, err)
	assert.Nil(t, res)
	assert.Contains(t, err.Error(), "revoke token")
	assert.Contains(t, err.Error(), "bar")
	assert.NotContains(t, err.Error(), "get token")

	mock.rerr = nil
	res, err = api.RevokeTokenHandler()
	assert.NoError(t, err)
	j, err := json.Marshal(res)
	assert.NoError(t, err)
	assert.Equal(t, `{"message":"revocation successful"}`, string(j))
}
