import Expression from './Expression'

const scoresToExpression = (scores, priors) => {
  const priorsAsExponent = priors.map(Math.exp)
  let res // This will be the result: the final expression.

  // Walk through all the teams and set up the integral for each one of them.
  scores.forEach((scoreList, i) => {
    // Set up factor for number of points scored.
    const totalScored = scoreList.reduce((sum, score) => sum + score, 0)
    const scoreFactor = (totalScored === 0 ?
      new Expression('number', 1) :
      new Expression('exponent', {
        base: new Expression('parameter', i + 1),
        power: totalScored,
      })) // x1^n

    // Set up the prior term in the denominator.
    const priorFactor = new Expression('exponent', {
      base: new Expression('sum', [
        new Expression('parameter', i + 1),
        new Expression('number', priorsAsExponent[i]),
      ]),
      power: 2,
    }) // (x1 + k1)^2

    // Add all the game points scored in the denominator.
    const teamFactors = [priorFactor]
    scoreList.forEach((score, j) => {
      if (j <= i)
        return // Ignore this if we already considered these two teams together.
      const scoredBetween = scores[i][j] + scores[j][i]
      if (scoredBetween === 0)
        return
      teamFactors.push(new Expression('exponent', {
        base: new Expression('sum', [
          new Expression('parameter', i + 1),
          new Expression('parameter', j + 1),
        ]),
        power: scoredBetween,
      }))
    })

    // Set up the full fraction.
    const fraction = new Expression('fraction', {
      numerator: scoreFactor,
      denominator: teamFactors.length === 1 ? teamFactors[0] : new Expression('product', teamFactors),
    })

    // Set up the integral, possibly including an earlier integral.
    res = (res ? new Expression('product', [fraction, res]) : fraction)
    res = new Expression('integral', {
      integrand: res,
      parameter: i + 1,
    })
  })

  // Include the constant prior-factor multiplication.
  const factor = priorsAsExponent.reduce((factor, priorExponent) => factor * priorExponent, 1)
  res = (factor === 1 ? res : new Expression('product', [new Expression('number', factor), res]))

  // All done!
  return res
}

export { scoresToExpression }