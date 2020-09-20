package weather

import (
	"time"

	"github.com/EmilGedda/turplanering/srv/internal/gis"
)

type Wind struct {
	// Unit: degrees, from 0 to 360
	Direction float32
	// Unit: m/s
	Speed float32
}

type Measurement struct {
	// Unit: Celsius
	Temp float32
	// Mean precipitation intensity. Unit: mm/h
	Precipitation float32
	Wind          Wind
}

type ForecastPoint struct {
	Coords gis.WGS84
	Measurement
}

type Warning struct {
	Message string
}

type WeatherService interface {
	ValidTimes() ([]time.Time, error)
	GetWarnings() ([]Warning, error)
	GetMeasurements(time.Time) ([]Measurement, error)
	GetPoints() ([]gis.WGS84, error)
}

type ForecastStorage interface {
	ValidTimes() ([]time.Time, error)
	GetForecast(gis.Area, time.Time) ([]ForecastPoint, error)
	GetWarnings(gis.Area) ([]Warning, error)
}
