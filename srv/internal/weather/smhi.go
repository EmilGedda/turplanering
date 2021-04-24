package weather

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"sync"
	"time"

	"github.com/EmilGedda/turplanering/srv/internal/errc"
	"github.com/EmilGedda/turplanering/srv/internal/gis"
	"github.com/EmilGedda/turplanering/srv/internal/util"
)

type api struct {
	category string
	version  int
}

func (a *api) url() string {
	return fmt.Sprintf("/api/category/%s/version/%d", a.category, a.version)
}

type param struct {
	assign      func(*Measurement, float32)
	description string
	name        string
	levelType   string
	level       string
}

func (p *param) url() string {
	return fmt.Sprintf("/parameter/%s/leveltype/%s/level/%s", p.name, p.levelType, p.level)
}

type HTTPGetter interface {
	Get(url string) (*http.Response, error)
}

type Smhi struct {
	client     HTTPGetter
	downsample int
	api        api
}

func (s *Smhi) url() string {
	const baseURL = "https://opendata-download-metfcst.smhi.se"
	return baseURL + s.api.url()
}

func (s *Smhi) GetPoints() ([]gis.WGS84, error) {
	type response struct {
		Coords [][2]float32 `json:"coordinates"`
	}

	endpoint := fmt.Sprintf("/geotype/multipoint.json?&downsample=%d", s.downsample)
	res, err := s.client.Get(s.url() + endpoint)
	if err != nil {
		return nil, err
	}

	defer res.Body.Close()

	data := &response{}
	d, _ := ioutil.ReadAll(res.Body)
	err = json.Unmarshal(d, data)
	if err != nil {
		return nil, &errc.UnmarshalError{
			Err:  err,
			Json: d,
			Obj:  data,
		}
	}

	points := make([]gis.WGS84, len(data.Coords))
	for k := range points {
		points[k] = gis.WGS84{
			Longitude: data.Coords[k][0],
			Latitude:  data.Coords[k][1],
		}
	}

	return points, nil
}

func (s *Smhi) ValidTimes() ([]time.Time, error) {
	type response struct {
		ValidTimes []time.Time `json:"validTime"`
	}

	res, err := s.client.Get(s.url() + "/validtime.json")
	if err != nil {
		return nil, err
	}

	defer res.Body.Close()

	if res.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("GET validtime.json failed with status code %s", res.Status)
	}

	data := &response{}
	err = json.NewDecoder(res.Body).Decode(data)
	if err != nil {
		return nil, err
	}

	if len(data.ValidTimes) == 0 {
		return nil, errors.New("no valid times returned from smhi")
	}

	return data.ValidTimes, nil
}

type measurementResponse struct {
	name   string
	values []float32
	assign func(*Measurement, float32)
	err    error
}

func (s *Smhi) GetMeasurements(timepoint time.Time) ([]Measurement, error) {

	timeUrl := "/geotype/multipoint/validtime/" + timepoint.Format("20060102T150405Z")
	endpoint := fmt.Sprintf("/data.json?with-geo=false&downsample=%d", s.downsample)

	parameters := []param{
		{func(m *Measurement, val float32) { m.Temp = val }, "temperature", "t", "hl", "2"},
		{func(m *Measurement, val float32) { m.Wind.Speed = val }, "wind speed", "ws", "hl", "10"},
		{func(m *Measurement, val float32) { m.Wind.Direction = val }, "wind direction", "wd", "hl", "10"},
		{func(m *Measurement, val float32) { m.Precipitation = val }, "precipitation", "pmean", "hl", "0"},
	}

	type smhiResponse struct {
		ApprovedTime  time.Time `json:"approvedTime"`
		ReferenceTime time.Time `json:"referenceTime"`
		TimeSeries    []struct {
			ValidTime  time.Time `json:"validTime"`
			Parameters []struct {
				Name      string    `json:"name"`
				LevelType string    `json:"levelType"`
				Level     int       `json:"level"`
				Unit      string    `json:"unit"`
				Values    []float32 `json:"values"`
			} `json:"parameters"`
		} `json:"timeSeries"`
	}

	weatherdata := make([]measurementResponse, len(parameters))

	wg := sync.WaitGroup{}
	for i := range parameters {
		i := i
		wg.Add(1)
		go func() {
			defer wg.Done()
			parameter := &parameters[i]
			ret := &weatherdata[i]

			res, err := s.client.Get(s.url() + timeUrl + parameter.url() + endpoint)
			if err != nil {
				ret.err = errc.Wrap(err, "failed to GET measurement "+parameter.description)
				return
			}

			defer res.Body.Close()

			if res.StatusCode != http.StatusOK {
				body, _ := ioutil.ReadAll(res.Body)
				ret.err = fmt.Errorf("GET measurements failed with status code %s: %s", res.Status, body)
				return
			}

			response := &smhiResponse{}
			err = json.NewDecoder(res.Body).Decode(response)
			if err != nil {
				ret.err = &errc.UnmarshalError{
					Err: err,
					Obj: response,
				}
				return
			}

			nSeries := len(response.TimeSeries)
			if nSeries != 1 {
				ret.err = fmt.Errorf("invalid number of elements in timeSeries, expected 1 but got %d", nSeries)
			}

			nParams := len(response.TimeSeries[0].Parameters)
			if nParams != 1 {
				ret.err = fmt.Errorf("invalid number of elements in timeSeries.parameters, expected 1 but got %d", nParams)
			}

			ret.name = parameter.description
			ret.assign = parameter.assign
			ret.values = response.TimeSeries[0].Parameters[0].Values
		}()
	}
	wg.Wait()

	err := collectErrors(weatherdata)
	if err != nil {
		return nil, err
	}

	numPoints, err := validatePointCount(weatherdata)
	if err != nil {
		return nil, err
	}

	// Aggregate measurements from weatherdata by point
	// Requires deterministic order of measurement data
	measurements := make([]Measurement, numPoints)
	for i := range measurements {
		for _, measurement := range weatherdata {
			m := &measurements[i]
			measurement.assign(m, measurement.values[i])
		}
	}

	return measurements, nil
}

type PointCountInfo struct {
	Parameter string `json:"parameter"`
	Numpoints int    `json:"numpoints"`
}

type InconsistentPointCount []PointCountInfo

func (e *InconsistentPointCount) Error() string {
	return "inconsistent number of points returned from smhi"
}

func validatePointCount(responses []measurementResponse) (int, error) {
	numPoints := len(responses[0].values)
	for _, v := range responses {
		if len(v.values) != numPoints {
			break
		}
	}

	if numPoints != len(responses[0].values) {
		data := []PointCountInfo{}
		for _, v := range responses {
			data = append(data, PointCountInfo{
				Parameter: v.name,
				Numpoints: len(v.values),
			})
		}

		err := InconsistentPointCount(data)
		return -1, &err
	}

	return numPoints, nil
}

func collectErrors(responses []measurementResponse) error {
	didErr := false
	for _, v := range responses {
		if v.err != nil {
			didErr = true
			break
		}
	}

	if !didErr {
		return nil
	}

	list := []error{}
	for _, v := range responses {
		if v.err != nil {
			list = append(list, errc.Wrap(v.err, "failed fetching measurement "+v.name))
		}
	}

	err := errc.CompoundError(list)
	return &err
}

type SMHIOpt = func(*Smhi)

func WithDownSampling(d int) SMHIOpt {
	return func(s *Smhi) { s.downsample = util.ClampInt(d, 1, 20) }
}
func WithGetter(g HTTPGetter) SMHIOpt { return func(s *Smhi) { s.client = g } }

func NewSmhi(opts ...SMHIOpt) *Smhi {
	smhi := &Smhi{
		client:     http.DefaultClient,
		api:        api{"pmp3g", 2},
		downsample: 1,
	}

	for _, opt := range opts {
		opt(smhi)
	}

	return smhi
}
