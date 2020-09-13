// +build integration

package auth

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"

	"github.com/EmilGedda/turplanering/srv/internal/env"
)

func TestIntegrationGetToken(t *testing.T) {
	l, err := NewLantmateriet()
	assert.NoError(t, err)

	first, err := l.GetToken()
	assert.NoError(t, err)
	second, err := l.GetToken()
	assert.NoError(t, err)
	assert.True(t, first.ExpiresAt.After(time.Now()))
	assert.Equal(t, first.AccessToken, second.AccessToken)
	assert.WithinDuration(t,
		first.ExpiresAt,
		second.ExpiresAt,
		1*time.Second,
	)

	l, err = NewLantmateriet(
		WithConsumerID("foo"),
		WithConsumerKey("bar"),
	)
	assert.NoError(t, err)

	_, err = l.GetToken()
	errType := "invalid_client"
	errDesc := "Client Authentication failed."
	assert.Equal(t, &TokenErrResponse{ErrorType: &errType, ErrorDescription: &errDesc}, err)
}

func TestIntegrationRevokeToken(t *testing.T) {
	l, err := NewLantmateriet()
	assert.NoError(t, err)

	first, err := l.GetToken()
	assert.NoError(t, err)
	assert.NoError(t, l.RevokeToken(first))
	assert.NoError(t, l.RevokeToken(first))
	second, err := l.GetToken()
	assert.NotEqual(t,
		first.AccessToken,
		second.AccessToken,
	)
	assert.True(t, second.ExpiresAt.After(first.ExpiresAt))
}
