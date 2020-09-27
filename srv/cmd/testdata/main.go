package main

import (
	"compress/gzip"
	"encoding/gob"
	"fmt"
	"os"
	"time"

	"github.com/rs/zerolog"
	flag "github.com/spf13/pflag"

	"github.com/EmilGedda/turplanering/srv/internal/weather"
)

func main() {
	zerolog.TimeFieldFormat = zerolog.TimeFormatUnixMicro
	zerolog.SetGlobalLevel(zerolog.DebugLevel)

	logger := zerolog.New(
		zerolog.ConsoleWriter{
			Out:        os.Stdout,
			TimeFormat: "15:04:05.000",
		},
	).With().Timestamp().Logger()

	args := map[string]func(*zerolog.Logger){
		"forecast": DownloadForecast,
	}

	flag.Parse()
	testdata := flag.Arg(0)

	if testdata == "" {
		flag.PrintDefaults()
	}

	f, ok := args[testdata]
	if !ok {
		fmt.Printf("unknown test data \"%v\"\n", testdata)
		fmt.Println("valid arguments are:")
		for k := range args {
			fmt.Println("\t", k)
		}
		return
	}

	f(&logger)
}

func DownloadForecast(logger *zerolog.Logger) {

	f, err := os.Create("forecast.gz")
	defer f.Close()
	if err != nil {
		logger.Err(err).
			Msg("Failed to prepare forecast.gz file")
		return
	}

	logger.Info().Msg("Downloading weather forecast...")
	smhi := weather.NewSmhi(weather.WithDownSampling(2))
	before := time.Now()
	data, err := smhi.GetForecast()
	elapsed := time.Now().Sub(before)
	if err != nil {
		logger.Err(err).Dur("elapsed", elapsed).
			Msg("Failed to download weather forecast")
		return
	}

	logger.Info().
		Str("elapsed", elapsed.Truncate(time.Millisecond).String()).
		Int("datapoints", len(data.Points)).
		Int("timestamps", len(data.Prognosis)).
		Msg("Downloaded weather forecast")

	logger.Info().
		Msg("Saving forecast to file forecast.gz...")

	gz := gzip.NewWriter(f)
	enc := gob.NewEncoder(gz)

	before = time.Now()
	enc.Encode(data)
	gz.Close()
	elapsed = time.Now().Sub(before)

	stat, _ := f.Stat()

	logger.Info().
		Str("elapsed", elapsed.Truncate(time.Millisecond).String()).
		Str("size", ByteCountSI(stat.Size())).
		Msg("Finished writing to forecast.gz")

}

func ByteCountSI(b int64) string {
	const unit = 1000
	if b < unit {
		return fmt.Sprintf("%dB", b)
	}
	div, exp := int64(unit), 0
	for n := b / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}

	return fmt.Sprintf("%.1f%cB", float64(b)/float64(div), "kMGTPE"[exp])
}
