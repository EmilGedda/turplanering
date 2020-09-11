package auth

import (
	"encoding/json"
	"io"
	"io/ioutil"
	"net/http"
	"strings"
	"testing"
	"time"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"

	"github.com/EmilGedda/turplanering/srv/internal/errc"
)

type reader struct {
	n   int
	err error
}

func (r *reader) Read(p []byte) (n int, err error) {
	return r.n, r.err
}

type mockClient struct {
	req *http.Request
	res *http.Response
	err error
}

func stringReader(s string) io.ReadCloser {
	return ioutil.NopCloser(strings.NewReader(s))
}

func (client *mockClient) Do(req *http.Request) (*http.Response, error) {
	client.req = req
	return client.res, client.err
}

var (
	emptyResponse http.Response = http.Response{
		Status:     "200 OK",
		StatusCode: 200,
		Body:       ioutil.NopCloser(strings.NewReader("")),
	}
	errReader reader = reader{err: errors.New("read error")}
)

func TestGetToken(t *testing.T) {

	empty := emptyResponse
	mock := &mockClient{
		err: errors.New("connection failure"),
		res: &empty,
	}

	l, _ := NewLantmateriet(
		WithClient(mock),
		WithConsumerID("a"),
		WithConsumerKey("a"),
	)

	_, err := l.GetToken()
	assert.Error(t, err, "request Do failed error")

	mock.err = nil
	_, err = l.GetToken()
	assert.Error(t, err, "empty body fails json marshalling")
	deserializeErr := &errc.UnmarshalError{}
	syntaxErr := &json.SyntaxError{}
	assert.True(t, errors.As(err, &deserializeErr))
	assert.True(t, errors.As(err, &syntaxErr))
	assert.Empty(t, deserializeErr.Json)

	mock.res.Body = ioutil.NopCloser(&errReader)
	_, err = l.GetToken()
	assert.Error(t, err, "failed to read body")

	mock.res.Body = stringReader("{}")
	_, err = l.GetToken()
	assert.Error(t, err, "missing required fields")

	mock.res.Body = stringReader(`{"access_token": "a", "expires_in": 1}`)
	token, err := l.GetToken()
	assert.NoError(t, err)
	assert.Equal(t, "a", token.AccessToken)
	assert.Equal(t, 1*time.Second, token.ExpiresIn)

	l, _ = NewLantmateriet(WithURL("\n"))
	_, err = l.GetToken()
	assert.Error(t, err, "invalid url")
}

func TestRevokeToken(t *testing.T) {

	empty := emptyResponse
	token := &Token{
		AccessToken: "a",
	}

	mock := &mockClient{
		err: errors.New("connection failure"),
		res: &empty,
	}

	l, _ := NewLantmateriet(
		WithClient(mock),
		WithConsumerID("a"),
		WithConsumerKey("a"),
	)
	brokenURL, _ := NewLantmateriet(WithURL("\n"))

	assert.Error(t, brokenURL.RevokeToken(token), "invalid url")

	assert.Error(t, l.RevokeToken(nil), "nil token")
	assert.Error(t, l.RevokeToken(token), "request Do failed error")

	mock.err = nil
	mock.res.Body = ioutil.NopCloser(&errReader)
	assert.Error(t, l.RevokeToken(token), "failed to read body")

	mock.res.Body = stringReader("{}")
	assert.Error(t, l.RevokeToken(token), "non-empty body")

	mock.res.Body = stringReader("")
	assert.NoError(t, l.RevokeToken(token))
}

func TestRefreshToken(t *testing.T) {
	empty := emptyResponse
	token := &Token{
		AccessToken: "a",
	}

	mock := &mockClient{
		err: errors.New("connection failure"),
		res: &empty,
	}

	l, _ := NewLantmateriet(
		WithClient(mock),
		WithConsumerID("a"),
		WithConsumerKey("a"),
	)

	_, err := l.RefreshToken(token)
	assert.Error(t, err)

}
