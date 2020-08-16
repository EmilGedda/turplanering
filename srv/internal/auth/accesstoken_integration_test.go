// +build integration

package auth

import (
	"github.com/stretchr/testify/assert"
	"testing"
	"time"
)

func TestIntegrationGetToken(t *testing.T) {
	l := NewLantmateriet()

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
}

func TestIntegrationRevokeToken(t *testing.T) {
	l := NewLantmateriet()

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
