import parseNumber from './parseNumber'

// combination returns (a above b) = a!/(b! * (a-b)!).
export default function combination(a, b) {
	// Check input.
	a = parseNumber(a)
	b = parseNumber(b)
  if (a < b)
    throw new RangeError(`Combination error: cannot take a combination of ${a} above ${b}. The first number may not be smaller than the second.`)

  // Calculate.
  b = Math.min(b, a - b) // For efficiency.
  let res = 1
  for (let i = 1; i <= b; i++) {
    res *= (a - b + i);
    res /= i;
  }
  return res
}