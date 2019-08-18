import Expression from './Expression'
import { permutation } from './util'

// Turns a set of games and prior team strengths (optional) into an expression that needs to be integrated over all teams.
const scoresToExpression = (games, priors) => {
  // Extract a team list from the games.
  const teamList = teamListFromGames(games)

  // For all the teams, set up priors.
  const productTerms = []
  teamList.forEach(team => {
    // Check prior input.
    if (priors && priors[team] === undefined)
      throw new Error(`Invalid priors: a list of prior team strengths was given, but team ${team} was not in that list.`)

    // Add the term k*u / (u + k)^2.
    const prior = priors ? priors[team] : 0
    const priorAsExponent = Math.exp(prior)
    productTerms.push(new Expression('number', priorAsExponent))
    // productTerms.push(new Expression('parameter', team)) // This one is not needed when working with the substituted variables u = e^s. It disappears in the substitution.
    productTerms.push(new Expression('exponent', {
      base: new Expression('sum', [
        new Expression('parameter', team),
        new Expression('number', priorAsExponent),
      ]),
      power: -2,
    }))
  })

  // For all the teams, set up the likelihood for that game.
  games.forEach(game => {
    productTerms.push(new Expression('number', permutation(game.points1 + game.points2, game.points1)))
    productTerms.push(new Expression('exponent', {
      base: new Expression('parameter', game.team1),
      power: game.points1,
    }))
    productTerms.push(new Expression('exponent', {
      base: new Expression('parameter', game.team2),
      power: game.points2,
    }))
    productTerms.push(new Expression('exponent', {
      base: new Expression('sum', [
        new Expression('parameter', game.team1),
        new Expression('parameter', game.team2),
      ]),
      power: -(game.points1 + game.points2),
    }))
  })

  // All done!
  return new Expression('product', productTerms)
}

// Turns games into an array [team1, team2, team3, ...].
const teamListFromGames = (games) => {
  const list = []
  const presence = {}
  games.forEach(game => {
    [game.team1, game.team2].forEach(team => {
      if (!presence[team]) {
        list.push(team)
        presence[team] = true
      }
    })
  })
  return list
}

export { scoresToExpression }
