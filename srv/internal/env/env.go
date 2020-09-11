package env

import (
	"sync"

	lib "github.com/caarlos0/env/v6"
)

type RunningEnvironment string

const (
	Production  RunningEnvironment = "production"
	Development RunningEnvironment = "development"
	Testing     RunningEnvironment = "testing"
)

type Environment struct {
	Env RunningEnvironment `env:"TURPLANERING_ENVIRONMENT"`
	*Lantmateriet
}

type Lantmateriet struct {
	ConsumerKey string `env:"LANTMATERIET_CONSUMERKEY"`
	ConsumerID  string `env:"LANTMATERIET_CONSUMERID"`
}

var (
	once        sync.Once
	environment Environment = Environment{
		Env:          Development,
		Lantmateriet: &Lantmateriet{},
	}
)

func Vars() Environment {
	once.Do(func() {
		err := lib.Parse(&environment)
		if err != nil {
			panic("error loading env")
		}

		// validate the running env
		switch environment.Env {
		case Production:
		case Development:
		case Testing:
		default:
			environment.Env = Development
		}

		// TODO: validation should not be here
		// create a config pkg which handles validation?
		// read from file in prod, and file + env otherwise

		// prevent using secrets from environment in production
		l := environment.Lantmateriet
		if environment.Env == Production && (l.ConsumerID != "" || l.ConsumerKey != "") {
			panic("Lantmäteriet credentials provided in environment in production")
		}
	})
	return environment
}
