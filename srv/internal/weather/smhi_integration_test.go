// +build integration

package weather

import (
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var smhi = NewSmhi(WithDownSampling(20))

func assertBetween(t *testing.T, lower, upper, obj float32) {
	// assert.InDelta is not descriptive enough in error messages
	assert.GreaterOrEqual(t, obj, lower, fmt.Sprintf("%v must be greater than %v", obj, lower))
	assert.LessOrEqual(t, obj, upper, fmt.Sprintf("%v must be less than %v", obj, upper))
}

func TestGetPoints(t *testing.T) {
	points, err := smhi.GetPoints()
	require.NoError(t, err)
	assert.Greater(t, len(points), 10)
	assert.Less(t, len(points), 1000000)

	for _, point := range points {
		assertBetween(t, 52, 72, point.Latitude)
		assertBetween(t, -9, 37, point.Longitude)
	}
}

func TestValidTimes(t *testing.T) {
	times, err := smhi.ValidTimes()
	require.NoError(t, err)

	assertBetween(t, 64, 76, float32(len(times)))

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
		assertBetween(t, -30, 40, measure.Temp)
		assertBetween(t, 0, 360, measure.Wind.Direction)
		assertBetween(t, 0, 40, measure.Wind.Speed)
		assertBetween(t, 0, 40, measure.Precipitation)
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
