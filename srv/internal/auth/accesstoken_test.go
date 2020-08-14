package auth

import (
	"fmt"
	"github.com/stretchr/testify/assert"
	"net/http"
	"testing"
	"time"
)

type mockClient struct {
	req *http.Request
	res *http.Response
	err error
}

func printToken(t *Token) {
	fmt.Printf(
		"Token{\n  AccessToken:\t%s\n  ExpiresIn:\t%v\n  ExpiresAt\t%s\n}\n",
		t.AccessToken,
		t.ExpiresIn.Seconds(),
		t.ExpiresAt.Format(time.UnixDate),
	)
}

func (client *mockClient) Do(req *http.Request) (*http.Response, error) {
	client.req = req
	return client.res, client.err
}

func TestIntegrationGetToken(t *testing.T) {
	l := NewLantmateriet()

	first, err := l.GetToken()
	assert.Nil(t, err)
	second, err := l.GetToken()
	assert.Nil(t, err)
	assert.True(t, first.ExpiresAt.After(time.Now()))
	assert.Equal(t, first.AccessToken, second.AccessToken)
	assert.WithinDuration(t, first.ExpiresAt, second.ExpiresAt, 500*time.Millisecond)
	assert.Nil(t, err)
}

func TestIntegrationRevokeToken(t *testing.T) {
	l := NewLantmateriet()

	first, err := l.GetToken()
	assert.Nil(t, err)
	assert.Nil(t, l.RevokeToken(first))
	assert.Nil(t, l.RevokeToken(first))
	second, err := l.GetToken()
	assert.NotEqual(t, first.AccessToken, second.AccessToken)
	assert.NotEqual(t, first.ExpiresAt.Format(time.UnixDate), second.ExpiresAt.Format(time.UnixDate))
}
