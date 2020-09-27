package weather

import (
	"context"
	"time"

	"github.com/EmilGedda/turplanering/srv/internal/gis"
)

type Storage struct {
	// 	conn *pgx.Conn
}

func (s *Storage) Query(context.Context, gis.Area) (*Forecast, error) {
	return nil, nil
}
func (s *Storage) Create(context.Context, []gis.WGS84) error {
	return nil
}
func (s *Storage) Update(context.Context, time.Time, []Measurement) error {
	return nil
}
