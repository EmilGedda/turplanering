package auth

import (
	"fmt"
	"net/http"
	"github.com/stretchr/testify/assert"
	"testing"
	"strings"
	"io/ioutil"
	"time"
)

// "github.com/stretchr/testify/assert"
// "testing"

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

var (
	errorResponse *http.Response = &http.Response{
			Status: "500 Internal Server Error",
			StatusCode: 500,
	}
	emptyResponse *http.Response = &http.Response{
			Status: "200 OK",
			StatusCode: 200,
			Body: ioutil.NopCloser(strings.NewReader("")),
	}

)

func TestGetToken(t *testing.T) {

	mock := &mockClient {
		err: fmt.Errorf("test"),
		res: emptyResponse,
	}

	l := NewLantmateriet(WithClient(mock))

	_, err := l.GetToken()
	assert.Error(t, err)
	mock.err = nil
	_, err = l.GetToken()
	assert.Error(t, err)
}

func TestRevokeToken(t *testing.T) {

	mock := &mockClient {
		err: fmt.Errorf("test"),
	}

	_ = NewLantmateriet(WithClient(mock))

	//first, err := l.GetToken()
	//require.NoError(t, err)
	//require.NoError(t, l.RevokeToken(first))
	//require.NoError(t, l.RevokeToken(first))
	//second, err := l.GetToken()
	//require.NoError(t, err)
	//assert.NotEqual(t,
	//	first.AccessToken,
	//	second.AccessToken,
	//)
	//assert.True(t, second.ExpiresAt.After(first.ExpiresAt))
}
