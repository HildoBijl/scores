const roundTo = (num, digits = 0) => {
  const f = Math.pow(10, digits)
  return Math.round(num * f) / f
}

// Return (a above b) = a!/(b! * (a-b)!).
const permutation = (a, b) => {
  // Check input.
  if (a < b)
    throw new Error(`Permutation error: cannot take a permutation of ${a} above ${b}. The first number must be smaller than the second.`)

  // Calculate.
  b = Math.min(b, a - b) // For efficiency.
  let res = 1
  for (let i = 1; i <= b; i++) {
    res *= (a - b + i);
    res /= i;
  }
  return res
}

export { roundTo, permutation }