package auth

// https://www.lantmateriet.se/sv/Kartor-och-geografisk-information/oppna-data/

import (
	"encoding/json"
	"github.com/EmilGedda/turplanering/srv/internal/env"
	"github.com/EmilGedda/turplanering/srv/internal/errc"
	"github.com/pkg/errors"
	"io/ioutil"
	"net/http"
	"strings"
	"time"
)

type Token struct {
	AccessToken string
	ExpiresAt   time.Time
	ExpiresIn   time.Duration
}

type TokenService interface {
	RevokeToken(token *Token) error
	RefreshToken(token *Token) (*Token, error)
	GetToken() (*Token, error)
}

type NetClient interface {
	Do(req *http.Request) (*http.Response, error)
}

func NewNetClient() *http.Client {
	return &http.Client{
		Timeout: time.Second * 10,
	}
}

type Lantmateriet struct {
	client      NetClient
	url         string
	consumerID  string
	consumerKey string
}

const LantmaterietURL = "https://api.lantmateriet.se"

type LantmaterietOption = func(l *Lantmateriet)

func WithClient(client NetClient) LantmaterietOption {
	return func(l *Lantmateriet) {
		l.client = client
	}
}

func WithURL(url string) LantmaterietOption {
	return func(l *Lantmateriet) { l.url = url }
}

func WithConsumerID(id string) LantmaterietOption {
	return func(l *Lantmateriet) { l.consumerID = id }
}

func WithConsumerKey(key string) LantmaterietOption {
	return func(l *Lantmateriet) { l.consumerKey = key }
}

func NewLantmateriet(opts ...LantmaterietOption) *Lantmateriet {
	vars := env.Vars().Lantmateriet

	l := &Lantmateriet{
		NewNetClient(),
		LantmaterietURL,
		vars.ConsumerID,
		vars.ConsumerKey,
	}

	for _, opt := range opts {
		opt(l)
	}

	return l
}

func (l *Lantmateriet) RevokeToken(token *Token) error {
	msg := strings.NewReader("token=" + token.AccessToken)
	request, err := http.NewRequest("POST", l.url+"/revoke", msg)
	if err != nil {
		return errors.Wrap(err, "creating request failed")
	}

	request.SetBasicAuth(l.consumerID, l.consumerKey)
	response, err := l.client.Do(request)
	if err != nil {
		return errors.Wrap(err, "revoke token request failed")
	}
	defer response.Body.Close()
	data, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return errors.Wrap(err, "read body failed")
	}

	body := string(data)
	if body != "" {
		return errors.New("expected empty body but got: " + body)
	}

	return nil
}

func (l *Lantmateriet) RefreshToken(token *Token) (*Token, error) {
	if l.RevokeToken(token) != nil {
		// log
	}
	return l.GetToken()
}

type tokenResponse struct {
	AccessToken string `json:"access_token"`
	Scope       string `json:"scope"`
	TokenType   string `json:"token_type"`
	ExpiresIn   int    `json:"expires_in"`
}

func (l *Lantmateriet) GetToken() (*Token, error) {
	tok := &Token{}
	request, err := http.NewRequest("POST", l.url+"/token", strings.NewReader("grant_type=client_credentials"))
	if err != nil {
		return tok, errors.Wrap(err, "creating request failed")
	}

	request.SetBasicAuth(l.consumerID, l.consumerKey)
	response, err := l.client.Do(request)
	if err != nil {
		return tok, errors.Wrap(err, "get token request failed")
	}
	defer response.Body.Close()
	jsonData, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return tok, errors.Wrap(err, "read body failed")
	}

	apiToken := &tokenResponse{}
	err = json.Unmarshal(jsonData, apiToken)
	if err != nil {
		return tok, errors.Wrap(&errc.UnmarshalError{err, jsonData, apiToken}, "failed unmarshal on get token response")
	}

	duration := time.Duration(apiToken.ExpiresIn) * time.Second
	return &Token{
		AccessToken: apiToken.AccessToken,
		ExpiresIn:   duration,
		ExpiresAt:   time.Now().Add(duration),
	}, nil
}

//type cachedToken struct {
//    token Token
//    err error
//}
//
//type tokenProducerUpdate int
//
//const (
//    refresh tokenProducerUpdate = iota,
//    invalidate,
//    stop
//)
//
//type TokenCache struct {
//    atomic.Value // cache the token response
//    token *Token
//    cond *sync.Cond
//    refresh <-chan tokenProducerUpdate
//}
//
//type tokenProducer struct {
//    lock *sync.RWMutex
//    cond *sync.Cond
//    refresh chan<- tokenProducerUpdate
//    fetcher TokenService
//}
//
//func NewTokenCache(fetcher TokenService) *TokenCache {
//    lock := sync.RWMutex{}
//    refresh := make(chan struct{})
//    cond := sync.NewCond(lock.RLocker())
//
//    service := &TokenCache{
//        fetcher.GetToken(),
//        cond,
//        refresh,
//    }
//
//    invalidateToken := func(state error) error {
//        lock.Lock()
//        defer cond.Broadcast()
//        defer lock.Unlock()
//        if service.token.AccessToken == "invalidated" { // check type of Err instead
//            return nil
//        }
//
//        err := fetcher.RevokeToken(service.token) // TODO: timeout
//        service.token.AccessToken = "invalidated"
//        if err == nil {
//            service.token.Err = state
//        } else {
//            service.token.Err = err
//        }
//        return err
//    }
//
//    // Token producer
//    go func() {
//        for {
//            select {
//            case request = <-refresh:
//
//                switch request {
//                case stop:
//                    invalidateToken(errors.New("Token producer stopped"))
//                    return
//                case invalidate:
//                    invalidateToken(errors.New("Token Revoked"))
//                    continue
//                case refresh:
//                    fallthrough
//                }
//
//                fallthrough
//
//            case <-time.After(service.token.ExpiresIn - 5 * time.Second):
//                oldToken := service.token
//                newToken := fetcher.GetToken()  // TODO: timeout / Context
//                lock.Lock()
//                service.token = newToken
//                lock.Unlock()
//                cond.Broadcast()
//                fetcher.RevokeToken(oldToken)
//            }
//        }
//    }()
//
//    return service
//}
//
//func (ts *TokenCache) GetToken() Token {
//    ts.cond.L.Lock()
//    defer ts.cond.L.Unlock()
//    return ts.response
//}
//
//func (ts *TokenCache) updateToken(update tokenProducerUpdate) Token {
//    prev := ts.token
//    ts.refresh <- update
//
//    ts.cond.L.lock()
//    for ts.token.Err == nil && ts.response == prev {
//        ts.cond.Wait()
//    }
//    defer ts.cond.L.Unlock()
//
//    return ts.token
//}
//
//func (ts *TokenCache) RefreshToken() Token {
//    return ts.updateToken(refresh)
//}
//
//func (ts *TokenCache) RevokeToken() (err error) {
//    return ts.updateToken(invalidate).Err
//}
//
//func (ts *TokenCache) Stop() error {
//    err := ts.updateToken(invalidate)
//    if err != nil {
//        ts.updateToken(stop)
//        return err
//    }
//    return ts.updateToken(stop)
//}
