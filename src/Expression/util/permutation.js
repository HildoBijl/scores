import parseNumber from './parseNumber'

// permutation returns a!/b!.
export default function permutation(a, b) {
	// Check input.
	a = parseNumber(a)
	b = parseNumber(b)
  if (a < b)
    throw new RangeError(`Permutation error: cannot take a permutation of ${a} above ${b}. The first number may not be smaller than the second.`)

  // Calculate.
  let res = 1
  for (let i = b + 1; i <= a; i++) {
    res *= i;
  }
  return res
}