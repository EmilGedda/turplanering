// +build integration

package weather

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var smhi = NewSmhi(WithDownSampling(20))

func TestGetPoints(t *testing.T) {
	points, err := smhi.GetPoints()
	require.NoError(t, err)
	assert.Greater(t, len(points), 10)
	assert.Less(t, len(points), 1000000)

	for _, point := range points {
		assert.InDelta(t, 62.5, point.Latitude, 10)
		assert.InDelta(t, 15, point.Longitude, 25)
	}
}

func TestValidTimes(t *testing.T) {
	times, err := smhi.ValidTimes()
	require.NoError(t, err)

	assert.InDelta(t, 70, len(times), 5)

	// is sorted and recent
	last := time.Now().Add(-2 * time.Hour)
	for _, timepoint := range times {
		assert.True(t, timepoint.After(last))
		last = timepoint
	}
}

func TestGetMeasurements(t *testing.T) {
	times, err := smhi.ValidTimes()
	require.NoError(t, err)
	m, err := smhi.GetMeasurements(times[0])
	require.NoError(t, err)

	for _, measure := range m {
		assert.InDelta(t, 10, measure.Temp, 40)
		assert.InDelta(t, 180, measure.Wind.Direction, 180)
		assert.InDelta(t, 20, measure.Wind.Speed, 20)
		assert.InDelta(t, 20, measure.Precipitation, 20)
	}
}

func BenchmarkGetMeasurements(b *testing.B) {
	smhi := NewSmhi()
	times, _ := smhi.ValidTimes()
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		smhi.GetMeasurements(times[0])
	}
}
