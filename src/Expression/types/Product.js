import { Constant, ExpressionWithTerms, FunctionExpression, Parameter, Power, Sum } from '..'
import { roundTo } from '../util'

export default class Product extends ExpressionWithTerms {
	simplify() {
		// console.log('Starting product simplification: ' + this.toString())
		// First simplify the individual terms.
		const terms = this.terms.map(term => term.simplify())

		// The goal now is to walk through all the terms, collecting identical terms and keeping track of their powers. First, let's set up some storage.
		let factor = this.factor
		const foundTerms = [] // This will collect all the terms that we will find. So in "3*x^2*(4*x*y^2)*z" it will collect "x", "y" and "z".
		const powers = [] // This will keep track of the corresponding powers. So in "3*x^2*(4*x*y^2)*z" it will eventually contain [3,2,1].

		// When we want to add a term to the above arrays, we will use this function.
		const addTerm = (term, power = 1) => {
			const index = foundTerms.findIndex(term2 => term.equals(term2, true))
			if (index === -1) {
				const termToAdd = term.clone()
				termToAdd.factor = 1
				foundTerms.push(termToAdd)
				powers.push(power)
			} else {
				powers[index] += power
			}
		}

		// When we encounter a term in our product, we use this function to check it.
		const processTerm = (term) => {
			factor *= term.factor
			switch (term.constructor) {
				case Constant:
					return

				case Parameter:
				case Sum:
					return addTerm(term)


				case Product: // When we find a product, walk through the terms and add them.
					return term.terms.forEach(processTerm)

				case Power:
					return addTerm(term.base, term.power)

				default:
					if (term instanceof FunctionExpression)
						return addTerm(term)
					throw new TypeError(`Product simplify error: the Product expression does not support expressions of type "${term.getType()}" yet.`)
			}
		}

		// Alright, time to iterate! This is where the magic happens.
		terms.forEach(processTerm)

		// We have enough data to set up the result. Gather all the collected terms into a terms array.
		const newTerms = []
		foundTerms.forEach((term, index) => {
			// Ignore powers of 0. This becomes 1 anyway.
			if (powers[index] === 0)
				return

			// Upon a power of 1, just add the term.
			if (powers[index] === 1) {
				newTerms.push(term)
				return
			}

			// Upone a higher power, turn it into a Power expression.
			newTerms.push(new Power(term, powers[index]))
		})

		// Do some boundary case checks.
		if (factor === 0)
			return new Constant(0)
		if (newTerms.length === 0)
			return new Constant(factor)
		if (newTerms.length === 1)
			return newTerms[0].multiplyBy(factor)

		// The next step is to sort the terms in the array. 
		const productSort = (term1, term2) => {
			// Check the number of functions in the terms.
			const numFunctionsDiff = term1.getNumFunctions() - term2.getNumFunctions()
			if (numFunctionsDiff !== 0)
				return numFunctionsDiff

			// Check the polynomial degree of the terms.
			const polynomialDegreeDiff = term1.getPolynomialDegree() - term2.getPolynomialDegree()
			if (polynomialDegreeDiff !== 0)
				return -polynomialDegreeDiff // Minus sign to get higher degrees first.

			// The rest doesn't really matter.
			return 0
		}
		newTerms.sort(productSort)

		// Everything in order! Return the simplified product.
		// console.log('Result of Product simplification: ' + new Product(newTerms, factor).toString())
		return new Product(newTerms, factor)
	}

	toString(omitMinus = false) {
		let preamble
		if (this.factor === 1 || (this.factor === -1 && omitMinus))
			preamble = ''
		else if (this.factor === -1)
			preamble = '-'
		else
			preamble = `${roundTo(omitMinus ? Math.abs(this.factor) : this.factor, 3)}*`
		return `${preamble}${this.terms.map(term => term.toString()).join('*')}`
	}

	getDerivative(parameter) {
		// Apply the product rule.
		parameter = this.verifyParameter(parameter)
		const sumTerms = []
		const terms = this.terms
		terms.forEach((term, termIndex) => {
			const termsCopy = terms.map(term => term.clone()) // Make a full copy (clone) of the terms array.
			termsCopy[termIndex] = term.getDerivative(parameter) // Replace the i'th term by its derivative.
			if (!termsCopy[termIndex].equals(new Constant(0))) // If the derivative is not zero ...
				sumTerms.push(new Product(termsCopy, this.factor)) // ... then add this to the resulting sum. Also keep the factor.
		})
		return new Sum(sumTerms).simplify()
	}

	getPolynomialDegree() {
		return this.terms.reduce((sum, term) => sum + term.getPolynomialDegree(), 0) // Return the sum of the polynomial degrees of the terms in this product.
	}

	// applyPFE performs a partial fraction expansion to the product, with respect to the given parameter, returning the sum in partial fraction form equaling this product. The given parameter must be a string, like "x".
	getPFE(parameter) {
		// Check the given parameter.
		parameter = this.verifyParameter(parameter)
		if (!this.dependsOn(parameter))
			throw new Error(`Tried to apply a partial fraction expansion, but the expression does not depend on the given parameter "${parameter}". The expression equals "${this.toString()}".`)

		// Make sure we start with a simplified product.
		const product = this.simplify()

		// Browse over all terms that have a negative exponent.
		const pfeTerms = [] // This will be the terms of the resulting sum.
		product.terms.forEach((term, termIndex) => {
			// Check if we need to do anything with this term.
			if (!(term instanceof Power))
				return // Ignore non-powers, because they are never in the denominator. (We need a negative power for the term to be in the denominator.)
			if (term.power >= 0)
				return // Ignore terms in the numerator.
			if (!term.dependsOn(parameter))
				return // Ignore terms not depending on the given parameter.

			// We have a term of the form (x + [something])^(-power). First verify that this is so, and then find the substitution "-[something]".
			if (!(term.base instanceof Sum))
				throw new Error(`PFE error: we have a term in the denominator which does not have a sum as its base.`)
			if (term.base.terms.length !== 2)
				throw new Error(`PFE error: we have a term in the denominator which does not have two elements in its sum.`)
			const element = term.base.terms.findIndex(sumTerm => sumTerm.equals(new Parameter(parameter), true))
			if (element === -1)
				throw new Error(`PFE error: could not find the PFE-parameter ${parameter} in the term "${term.base.toString()}".`)
			const substitution = term.base.terms[1 - element].multiplyBy(-1 / term.base.terms[element].factor) // Take the other element. Make sure to clone it, to prevent accidentally changing the original. Also multiply it. So if we have (2*x + 3*y), make sure to get -3y/2.

			// Next, apply the substitution to find the PFE terms.
			// console.log('Considering the term ' + term.toString())
			let productWithoutTerm = new Product(product.terms.filter((_, index) => index !== termIndex)) // Copy the terms array and cut out the term we're focusing on now for the PFE.
			for (let i = 0; i < -term.power; i++) {
				// console.log('- Taking the remainder ' + productWithoutTerm.toString())
				// If needed, take the derivative of this term.
				if (i > 0) {
					productWithoutTerm = productWithoutTerm.getDerivative(parameter)
					// console.log('- Derivative is: ' + productWithoutTerm.toString())
				}

				// Insert the right value for the parameter.
				// console.log('- Substituting ' + parameter + ' for ' + substitution.toString())
				const coefficient = productWithoutTerm.clone().substitute(parameter, substitution).simplify()
				// console.log('- Coefficient for ' + new Power(term.base.clone(), term.power + i, term.factor).toString() + ' is: ' + coefficient.toString())

				// Set up the relevant term. For this, multiply the coefficient with the relevant power term. Add this expression to the PFE sum.
				const exponent = new Power(term.base.clone(), term.power + i, term.factor)
				pfeTerms.push(new Product([coefficient, exponent]).simplify())
			}
		})

		// Replace this expression by the resulting PFE.
		return new Sum(pfeTerms, this.factor)
	}

	// hasDenominator checks if the product has terms with a negative power.
	hasDenominator() {
		return this.terms.some(term => (term instanceof Power && term.power < 0))
	}

	// expand expands the brackets inside the multiplication. Afterwards, a simplify is also automatically called.
	expand() {
		// Check if there is a denominator. It cannot be expanded then.
		if (this.hasDenominator())
			throw new Error(`Cannot expand a product with a denominator.`)

		// Walk through the terms and, if there is a sum, expand the multiplication brackets.
		let newTerms = [new Constant(1)]
		this.terms.forEach(term => {
			term = (term instanceof Power ? term.expand() : term) // If this is a power, expand that power into a sum first.
			let termsToIncorporate = (term instanceof Sum ? term.terms : [term]) // If we don't have a sum but a single multiplying term, just make an array of that element for easy processing.
			const oldTerms = newTerms // Switch array references.
			newTerms = []
			oldTerms.forEach(oldTerm => {
				termsToIncorporate.forEach(termToIncorporate => {
					newTerms.push(new Product([oldTerm, termToIncorporate], term.factor))
				})
			})
		})
		return new Sum(newTerms, this.factor).simplify()
	}

	integrateOver(parameter) {
		// Check that this term can be integrated. Only one term is allowed to depend on the given parameter.
		// console.log('Integrating product ' + this.toString())
		parameter = this.verifyParameter(parameter)
		const numDependentTerms = this.terms.reduce((sum, term) => sum + (term.dependsOn(parameter) ? 1 : 0), 0)
		if (numDependentTerms === 0)
			throw new Error(`Expression integrateOver error: we tried to integrate an expression "${this.toString()}" that did not depend on the given parameter "${parameter}".`)
		if (numDependentTerms > 1)
			throw new Error(`Expression integrateOver error: we tried to integrate an expression "${this.toString()}" that had too many dependencies on the given parameter "${parameter}".`)

		// We can integrate it! Let's integrate over only the dependent term, leaving the rest.
		const termIndex = this.terms.findIndex(term => term.dependsOn(parameter))
		const result = this.clone()
		result.terms[termIndex] = result.terms[termIndex].integrateOver(parameter)
		return result.simplify()
	}
}