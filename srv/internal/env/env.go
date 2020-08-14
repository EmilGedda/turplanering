package env

import (
	lib "github.com/caarlos0/env"
	"sync"
)

type Environment struct {
	*Lantmateriet
}

type Lantmateriet struct {
	ConsumerKey string `env:"LANTMATERIET_CONSUMERKEY"`
	ConsumerID  string `env:"LANTMATERIET_CONSUMERID"`
}

var (
	once        sync.Once
	environment Environment = Environment{
		&Lantmateriet{},
	}
)

func Vars() Environment {
	once.Do(func() {
		err := lib.Parse(&environment)
		if err != nil {
			panic("error loading env")
		}
	})
	return environment
}
