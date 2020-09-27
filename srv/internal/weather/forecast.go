package weather

import (
	"context"
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

// Forecast represents the prognosis of an area over the given times
// The Measurements in Prognosis values corresponds to the coordinate with
// the same index in the Points slice.
type Forecast struct {
	Points    []gis.WGS84
	Prognosis map[time.Time][]Measurement
}

// WeatherProvider fetches forecast data from an external provider.
type WeatherProvider interface {
	// Retrieves the list of valid timepoint for which there is a forecast.
	ValidTimes() ([]time.Time, error)
	// Gets all forecasted measurements on the given time.
	// The given timepoint must have been returned from a ValidTimes call.
	GetMeasurements(time.Time) ([]Measurement, error)
	// Retrieves all the geographical points for which any forecast is valid.
	// The boundary is Norway to Finland.
	GetPoints() ([]gis.WGS84, error)
}

type ForecastProvider interface {
	Query(context.Context, gis.Area) (*Forecast, error)
}

// ForecastService wraps WeatherService and ForecastStorage
type ForecastStorage interface {
	ForecastProvider
	Empty(context.Context) (bool, error)
	Create(context.Context, []gis.WGS84) error
	Update(context.Context, time.Time, []Measurement) error
}
