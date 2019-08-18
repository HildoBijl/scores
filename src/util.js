const roundTo = (num, digits = 0) => {
  const f = Math.pow(10, digits)
  return Math.round(num * f) / f
}

export { roundTo }