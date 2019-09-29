// This is the abstract ExpressionWithTerms class. It should not be instantiated, but it is used for Sum, Product and such.

import { Expression } from '..'

export default class ExpressionWithTerms extends Expression {
	constructor(terms, factor = 1) {
		// Check and store input.
		super(factor)
		if (!Array.isArray(terms))
			throw new TypeError(`Invalid terms parameter: tried to create a ${this.constructor.name}, but the terms parameter was not an array. Its value was "${terms}".`)
		terms.forEach((term, index) => {
			if (!(term instanceof Expression))
				throw new TypeError(`Invalid term: tried to create a ${this.constructor.name}, but the terms with index ${index} was not an Expression. Its value was "${term}".`)
		})
		this.terms = terms

		// Set parent for the children.
		const parent = this
		this.terms.forEach(term => { term.parent = parent })
	}

	equals(expression, ignoreFactor = false) { // Note that 2*(3*x)*y does not equal 6*x*y according to this function. Always simplify expressions before checking for equality.
		// Check that we have an expression of an equal number of terms and with equal factor.
		if (expression.constructor !== this.constructor)
			return false
		if (expression.terms.length !== this.terms.length)
			return false
		if (!ignoreFactor && expression.factor !== this.factor)
			return false

		// Walk through the terms and try to find a matching.
		const found = this.terms.map(() => false)
		return this.terms.every(term1 => { // For ever term, find a matching partner.
			const index = expression.terms.findIndex((term2, index) => !found[index] && term1.equals(term2)) // Is there a partner that has not been matched yet?
			if (index === -1)
				return false // No match found. Abort.
			found[index] = true // Note that this term has been matched.
			return true // Match found. Continue.
		})
	}

	dependsOn(parameter) {
		return this.terms.some(term => term.dependsOn(parameter))
	}

	getParameters() {
		let set = new Set()
		this.terms.forEach(term => {
			set = new Set([...set, ...term.getParameters()])
		})
		return [...set].sort()
	}
	
	substitute(parameter, substitution) {
		return new this.constructor(this.terms.map(term => term.substitute(parameter, substitution)), this.factor)
	}

	clone(deep = true) {
		return new this.constructor(this.terms.map(term => (deep ? term.clone(deep) : term)), this.factor)
	}

	getNumFunctions() {
		return this.terms.reduce((sum, term) => sum + term.getNumFunctions(), 0)
	}
}