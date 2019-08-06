import React from 'react'
import logo from './logo.svg'
import './App.css'
import { isArray } from 'util'

function App() {
  window.e = Expression
  return (
    <div className="App">
      <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <p>
          Edit <code>src/App.js</code> and save to reload.
        </p>
        <a
          className="App-link"
          href="https://reactjs.org"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn React
        </a>
      </header>
    </div>
  )
}

export default App

// Steps:
// [Done] Make an equation object, with various terms.
// [Done] Printing it.
// Partial fraction expansion: take a fraction and expand it to a partial fraction expansion. This does require
// - Simplifying fractions.
// - Taking derivatives of fractions.
// - Inserting values into fractions.
// It will be really hard...

class Expression {
  constructor(type, contents) {
    this.type = type
    switch (type) {
      case 'number': // [Number]
        const number = parseFloat(contents)
        if (isNaN(number) || !isFinite(contents))
          throw new Error(`Invalid Expression: a number-type expression should have a number as contents.`)
        this.number = number
        break

      case 'parameter': // [Positive integer]
        const parameter = parseInt(contents)
        Expression.checkParameter(parameter)
        this.parameter = parameter
        break

      case 'integral': // { integrand: [Expression], parameter: [Positive integer] }
        Expression.checkExpression(contents.integrand)
        Expression.checkParameter(contents.parameter)
        this.integrand = contents.integrand
        this.parameter = contents.parameter
        break

      case 'sum': // Array of [Expression] objects.
      case 'product':
        if (!isArray(contents))
          throw new Error(`Invalid Expression: a ${type}-type expression should have an array of terms as contents. A type "${typeof (contents)}" was found instead.`)
        contents.forEach(Expression.checkExpression)
        this.terms = contents
        break

      case 'fraction': // { numerator: [Expression], denominator: [Expression] }
        Expression.checkExpression(contents.numerator)
        Expression.checkExpression(contents.denominator)
        this.numerator = contents.numerator
        this.denominator = contents.denominator
        break

      case 'exponent': // { base: [Expression], power: [Positive integer] }
        Expression.checkExpression(contents.base)
        const power = parseInt(contents.power)
        if (isNaN(power) || !isFinite(contents.power))
          throw new Error(`Invalid Expression: an exponent must have a number as its power.`)
        if (!Number.isInteger(power) || power < 1)
          throw new Error(`Invalid Expression: an exponent-type expression must have a positive integer number as its power.`)
        this.base = contents.base
        this.power = power
        break

      case 'ln': // [Expression]
        Expression.checkExpression(contents)
        this.contents = contents
        break

      default:
        throw new Error(`Invalid Expression type "${type}" given.`)
    }
  }

  static checkParameter(parameter) {
    if (!Number.isInteger(parameter))
      throw new Error(`Invalid Expression: the given parameter should be an integer number.`)
    if (parameter < 1)
      throw new Error(`Invalid Expression: the given parameter should be a positive number.`)
  }

  static checkExpression(expression) {
    if (expression === undefined || expression === null)
      throw new Error(`Invalid Expression: the given expression did not exist.`)
    if (expression.constructor !== Expression)
      throw new Error(`Invalid Expression: the given expression was not of type "Expression".`)
  }

  print() {
    console.log(this.toString())
  }

  toString() {
    switch (this.type) {
      case 'number':
        return roundTo(this.number, 2)

      case 'parameter':
        return `[${this.parameter}]`

      case 'integral':
        return `int ${this.integrand.toString()} d[${this.parameter}]`

      case 'sum':
        return this.terms.map(term => term.toString()).join(' + ')

      case 'product':
        return this.terms.map(term => term.toString()).join(' * ')

      case 'fraction':
        return `(${this.numerator.toString()}) / (${this.denominator.toString()})`

      case 'exponent':
        const addBrackets = !(this.base.type === 'number' || this.base.type === 'parameter')
        if (addBrackets)
          return `(${this.base.toString()})^${this.power}`
        return `${this.base.toString()}^${this.power}`

      case 'ln':
        return `ln(${this.contents.toString()})`

      default:
        throw new Error(`Invalid expression type "${this.type}".`)
    }
  }
}

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

const roundTo = (num, digits = 0) => {
  const f = Math.pow(10, digits)
  return Math.round(num * f) / f
}

// const scores = [ // The number of points which teams have scored against each other. (The diagonal is ignored.)
//   [0, 0],
//   [1, 0],
// ]
// const priors = [0, 0] // The prior strength averages.

const scores = [ // The number of points which teams have scored against each other. (The diagonal is ignored.)
  [0, 4, 2],
  [1, 0, 3],
  [3, 1, 0],
]
const priors = [0.2, 0.05, -0.1] // The prior strength averages.

const e = scoresToExpression(scores, priors)
e.print()