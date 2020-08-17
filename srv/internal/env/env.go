package env

import (
	"sync"

	lib "github.com/caarlos0/env"
)

type Environment struct {
	Env string `env:"TURPLANERING_ENVIRONMENT"`
	*Lantmateriet
}

type Lantmateriet struct {
	ConsumerKey string `env:"LANTMATERIET_CONSUMERKEY"`
	ConsumerID  string `env:"LANTMATERIET_CONSUMERID"`
}

var (
	once        sync.Once
	environment Environment = Environment{
		Env:          "testing",
		Lantmateriet: &Lantmateriet{},
	}
)

func Vars() Environment {
	once.Do(func() {
		err := lib.Parse(&environment)
		if err != nil {
			panic("error loading env")
		}

		// prevent using secrets from environment other than for testing
		if environment.Env != "testing" {
			environment.Lantmateriet = &Lantmateriet{}
		}
	})
	return environment
}
