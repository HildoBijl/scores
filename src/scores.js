import { Constant, Parameter, Power, Product, Sum } from './Expression/index.js'
import { combination } from './Expression/util'

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

		// Add the term k / (u + k)^2.
		const prior = priors ? priors[team] : 0
		const priorAsExponent = Math.exp(prior)
		productTerms.push(new Constant(priorAsExponent))
		productTerms.push(new Power(new Sum([new Parameter(team), new Constant(priorAsExponent)]), -2))
	})

	// For all the games, set up the likelihood for that game.
	games.forEach(game => {
		productTerms.push(new Constant(combination(game.points1 + game.points2, game.points1)))
		productTerms.push(new Power(new Parameter(game.team1), game.points1))
		productTerms.push(new Power(new Parameter(game.team2), game.points2))
		productTerms.push(new Power(new Sum([new Parameter(game.team1), new Parameter(game.team2)]), -(game.points1 + game.points2)))
	})

	// All done!
	return new Product(productTerms)
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
