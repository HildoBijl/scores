import parseNumber from './parseNumber'
import parseInteger from './parseInteger'

// roundTo will round the given number to the desired number of significant digits. So roundTo(0.0314159265, 5) will give 0.031416 and roundTo(147, -1) will give 150.
export default function roundTo(num, digits = 0) {
	// Check input.
	num = parseNumber(num)
	digits = parseInteger(digits)

	// Boundary case check.
	if (num === 0)
		return 0

	// Calculate rounded number.
	const f = Math.pow(10, digits - Math.ceil(Math.log10(Math.abs(num))))
	if (f < 1)
		return Math.round(num)
  return Math.round(num * f) / f
}