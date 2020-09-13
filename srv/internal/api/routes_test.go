package api

import (
	"testing"

	"github.com/EmilGedda/turplanering/srv/internal/auth"
)

type mockTokenService struct {
	token auth.Token
	err   error
}

func (m *mockTokenService) GetToken() (*auth.Token, error) {
	return &m.token, m.err
}

func (m *mockTokenService) RevokeToken(*auth.Token) error {
	return m.err
}

func (m *mockTokenService) RefreshToken(*auth.Token) (*auth.Token, error) {
	return &m.token, m.err
}

func TestGetTokenHandler(t *testing.T) {
}

func TestRevokeTokenHandler(t *testing.T) {

}

func TestRefreshTokenHandler(t *testing.T) {
}
