import React from 'react'
import logo from './logo.svg'
import './App.css'
import { scoresToExpression as scoresToExpression1 } from './scores1.js'
import { scoresToExpression } from './scores.js'
import { Constant, Expression, Ln, Parameter, Power, Product, Sum } from './Expression'

function App() {
  window.Constant = Constant
  window.Expression = Expression
  window.Parameter = Parameter
  window.Product = Product
  window.Sum = Sum
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

let games, priors
const difficulty = 1

if (difficulty === 1) {
  games = [
    {
      team1: "UP",
      team2: "FE",
      points1: 1,
      points2: 0,
    },
  ]
  
  priors = {
    UP: 0,
    FE: 0,
  }
} else {
  games = [
    {
      team1: "UP",
      team2: "FE",
      points1: 4,
      points2: 1,
    },
    {
      team1: "UP",
      team2: "GD",
      points1: 2,
      points2: 3,
    },
    {
      team1: "FE",
      team2: "GD",
      points1: 3,
      points2: 1,
    },
  ]
  
  priors = {
    UP: 0.2,
    FE: 0.05,
    GD: -0.1,
  }
}

// console.log('Setting up first expression')
// const e = scoresToExpression1(games, priors)
// e.print()
// console.log('Simplifying the expression')
// e.simplify()
// e.print()
// console.log('Applying PFE over [UP]')
// e.applyPFE('UP')
// e.print()
// console.log('Integrating over [UP]')
// e.integrateOver('UP')
// e.print()

// e.findFirstChildOfType('integral').findFirstChildOfType('integral').findFirstChildOfType('integral').solveIntegrals(true)
// TODO: REINSTATE.
// e.solveIntegrals()

// const b = new Product([new Constant(3), new Constant(5), new Parameter('x'), new Parameter('y',2)], 7)
// b.print()
// const a = new Sum([new Constant(3), b, new Parameter('x'), new Parameter('y',2)])
// a.print()
// const c = a.getDerivative('y')
// c.print()
// const d = new Sum([new Constant(-3), new Parameter('y',-2), new Parameter('x',-3)])
// const e = new Sum([new Parameter('y',-2), new Constant(-3), new Parameter('x',-3)])
// console.log(d.equals(e))
// console.log(d)
// d.print()
// const f = d.clone()
// console.log(f)
// f.print()
// f.substitute('x',new Parameter('z',3)).print()
// f.print()

// const g = new Power(new Power(new Parameter('x'), 2, 2), 3, 3) // 3*(2*x^2)*3 becomes 24*x^6.
// const g = new Power(new Product([new Constant(3), new Power(new Parameter('x'),2), new Parameter('y',2)]), 3, 2) // 2*(3*x^2*2*y)^3 becomes 432*x^6*y^3.
// const g = new Sum([new Power(new Parameter('x'), 2, 3), new Parameter('y'), new Product([new Constant(2), new Power(new Parameter('x'), 2)]), new Parameter('x',4)], 3) // 3*(3*x^2 + y + 2*x^2 + 4*x) becomes 15*x^2 + 3*y + 12*x.
// const g = new Ln(new Sum([new Power(new Parameter('x'), 2), new Constant(1)],3)) // ln(3*(x^2 + 1))
// const g = new Ln(new Power(new Sum([new Parameter('x'), new Constant(1)]), 3, 2), 4) // 4*ln(2*(x+1)^3) should become 12*ln(2^(1/3)*(x+1)).
// const g = new Sum([new Power(new Parameter('x'),2), new Power(new Sum([new Parameter('x'), new Constant(2)], 4), -2), new Power(new Sum([new Parameter('x'), new Constant(2)], 4), 3)])
// g.print()
// g.simplify().print()
// g.simplify().getDerivative().print()
// g.getDerivative().print()

const parameter1 = 'FE'
const parameter2 = 'UP'
console.log('Setting up first expression')
const e = scoresToExpression(games, priors)
console.log('Expression: ' + e.toString())
console.log('Simplified: ' + e.simplify())
const f = e.simplify().getPFE(parameter1)
console.log('The PFE wrt ' + parameter1 + ' is: ' + f.toString())
// console.log('The simplified PFE is: ' + f.simplify().toString())
const g = f.integrateOver(parameter1)
console.log('Integrating over ' + parameter1 + ': ' + g.toString())
console.log('Next step: integrate this too, with respect to ' + parameter2 + '.')
// const h = g.getPFE(parameter2) // This fails at the moment.
// g.integrateOver(parameter2)

// const h = new Product([new Power(new Sum([new Parameter('y'), new Constant(1)]), -2), new Parameter('x'), new Power(new Sum([new Parameter('x'), new Parameter('y')]), -1)])
// h.print()
// h.getDerivative().print()

// const a = new Product([new Parameter('z'), new Power(new Sum([new Parameter('z'), new Parameter('a')]), -2)])
// a.print()
// console.log(a.getParameters())
// a.getDerivative('z').print()
// const b = a.getPFE('z')
// b.print()
// b.simplify().print()

// const a = new Product([new Sum([new Parameter('x'), new Constant(-2)]), new Power(new Sum([new Parameter('x', 2), new Constant(3)], 3), 4, 5)])
// a.print()
// a.simplify().print()
// a.expand().print()
