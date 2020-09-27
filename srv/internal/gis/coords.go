package gis

type SWEREF99TM struct {
	X int
	Y int
}

type WGS84 struct {
	Longitude float32
	Latitude  float32
}

type Area struct {
	TopLeft     WGS84
	BottomRight WGS84
}
