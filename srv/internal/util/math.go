package util

func ClampInt(val, lower, upper int) int {
	if val < lower {
		return lower
	}
	if val > upper {
		return upper
	}
	return val
}
