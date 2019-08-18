import React from 'react'
import logo from './logo.svg'
import './App.css'
import { scoresToExpression } from './scores.js'

function App() {
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


const games = [
  {
    team1: "UP",
    team2: "FE",
    points1: 1,
    points2: 0,
  },
]

const priors = {
  UP: 0,
  FE: 0,
}

// const games = [
//   {
//     team1: "UP",
//     team2: "FE",
//     points1: 4,
//     points2: 1,
//   },
//   {
//     team1: "UP",
//     team2: "GD",
//     points1: 2,
//     points2: 3,
//   },
//   {
//     team1: "FE",
//     team2: "GD",
//     points1: 3,
//     points2: 1,
//   },
// ]

// const priors = {
//   UP: 0.2,
//   FE: 0.05,
//   GD: -0.1,
// }

console.log('Setting up first expression')
const e = scoresToExpression(games, priors)
e.print()
console.log('Simplifying the expression')
e.simplify()
e.print()
console.log('Applying PFE over [UP]')
e.applyPFE('UP')
e.print()
// console.log('Integrating over [UP]')
// e.integrateOver('UP')
// e.print()

// e.findFirstChildOfType('integral').findFirstChildOfType('integral').findFirstChildOfType('integral').solveIntegrals(true)
// TODO: REINSTATE.
// e.solveIntegrals()