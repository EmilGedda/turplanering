package api

import (
	"encoding/json"
	"log"
	"net/http"
)

func (a *API) RefreshTokenHandler(w http.ResponseWriter, r *http.Request) {
	token, err := a.ts.GetToken()
	if err != nil {
		w.WriteHeader(500)
		return
	}

	token, err = a.ts.RefreshToken(token)
	if err != nil {
		w.WriteHeader(500)
		return
	}

	bytes, err := json.Marshal(token)
	if err != nil {
		w.WriteHeader(500)
		return
	}

	_, _ = w.Write(bytes)
}

func (a *API) RevokeTokenHandler(w http.ResponseWriter, r *http.Request) {
	token, err := a.ts.GetToken()
	if err != nil {
		w.WriteHeader(500)
	}

	err = a.ts.RevokeToken(token)
	if err != nil {
		w.WriteHeader(500)
	}
}

func (a *API) GetTokenHandler(w http.ResponseWriter, r *http.Request) {
	token, err := a.ts.GetToken()
	if err != nil {
		w.WriteHeader(500)
		log.Println(err)
		return
	}

	bytes, err := json.Marshal(token)
	if err != nil {
		w.WriteHeader(500)
		log.Println(err)
		return
	}

	_, _ = w.Write(bytes)
}
