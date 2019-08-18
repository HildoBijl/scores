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
e.solveIntegrals()