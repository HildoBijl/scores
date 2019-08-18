import { roundTo } from './util.js'

export default class Expression {
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
				Expression.checkParameter(contents)
				if (typeof (contents) === 'object') {
					this.parameter = contents.parameter
					this.positive = contents.positive
				} else {
					this.parameter = contents
					this.positive = true
				}
				break

			case 'integral': // { integrand: [Expression], parameter: [Positive integer] }
				Expression.checkExpression(contents.integrand)
				Expression.checkParameter(contents.parameter)
				this.integrand = contents.integrand
				this.parameter = contents.parameter
				this.integrand.parent = this
				break

			case 'sum': // Array of [Expression] objects.
			case 'product':
				if (!Array.isArray(contents))
					throw new Error(`Invalid Expression: a ${type}-type expression should have an array of terms as contents. A type "${typeof (contents)}" was found instead.`)
				contents.forEach(Expression.checkExpression)
				this.terms = contents
				const parent = this
				this.terms.forEach(term => { term.parent = parent })
				break

			case 'fraction': // { numerator: [Expression], denominator: [Expression] }
				Expression.checkExpression(contents.numerator)
				Expression.checkExpression(contents.denominator)
				this.numerator = contents.numerator
				this.denominator = contents.denominator
				this.numerator.parent = this
				this.denominator.parent = this
				break

			case 'exponent': // { base: [Expression], power: [Positive integer] }
				Expression.checkExpression(contents.base)
				const power = parseInt(contents.power)
				if (isNaN(power) || !isFinite(contents.power))
					throw new Error(`Invalid Expression: an exponent must have a number as its power.`)
				if (!Number.isInteger(power))
					throw new Error(`Invalid Expression: an exponent-type expression must have an integer number as its power.`)
				this.base = contents.base
				this.power = power
				this.base.parent = this
				break

			case 'ln': // [Expression]
				Expression.checkExpression(contents)
				this.contents = contents
				this.contents.parent = this
				break

			default:
				throw new Error(`Invalid Expression type "${type}" given.`)
		}
	}

	become(expression) {
		// First fully erase itself.
		delete this.number
		delete this.parameter
		delete this.positive
		delete this.integrand
		delete this.terms
		delete this.numerator
		delete this.denominator
		delete this.base
		delete this.power
		delete this.contents

		// Next, become the given expression.
		this.type = expression.type
		switch (expression.type) {
			case 'number':
				this.number = expression.number
				break

			case 'parameter':
				this.parameter = expression.parameter
				this.positive = expression.positive
				break

			case 'integral':
				this.integrand = expression.integrand
				this.parameter = expression.parameter
				this.integrand.parent = this
				break

			case 'sum': // Array of [Expression] objects.
			case 'product':
				this.terms = expression.terms
				const parent = this
				this.terms.forEach(term => { term.parent = parent })
				break

			case 'fraction':
				this.numerator = expression.numerator
				this.denominator = expression.denominator
				this.numerator.parent = this
				this.denominator.parent = this
				break

			case 'exponent':
				this.base = expression.base
				this.power = expression.power
				this.base.parent = this
				break

			case 'ln': // [Expression]
				this.contents = expression.contents
				this.contents.parent = this
				break

			default:
				throw new Error(`Invalid Expression type "${expression.type}" given to the become function.`)
		}

		// Return itself to allow for chaining.
		return this
	}

	clone(deep = true) {
		switch (this.type) {
			case 'number':
				return new Expression('number', this.number)

			case 'parameter':
				return new Expression('parameter', { parameter: this.parameter, positive: this.positive })

			case 'integral':
				return new Expression('integral', {
					integrand: deep ? this.integrand.clone(deep) : this.integrand,
					parameter: this.parameter,
				})

			case 'sum':
			case 'product':
				return new Expression(this.type, this.terms.map(term => (deep ? term.clone(deep) : term)))

			case 'fraction':
				return new Expression('fraction', {
					numerator: deep ? this.numerator.clone(deep) : this.numerator,
					denominator: deep ? this.denominator.clone(deep) : this.denominator,
				})

			case 'exponent':
				return new Expression('exponent', {
					base: deep ? this.base.clone(deep) : this.base,
					power: this.power,
				})

			case 'ln':
				return new Expression('ln', deep ? this.contents.clone(deep) : this.contents)

			default:
				throw new Error(`Invalid Expression type "${this.type}" to clone.`)
		}
	}

	// Check functions.

	static checkParameter(parameter) {
		// If the parameter is an object, it must follow a format. Otherwise it can be anything, like a string or a number.
		if (typeof (parameter) === 'object') {
			if (parameter.parameter === undefined)
				throw new Error(`Invalid Parameter: for a parameter-type expression, if the contents are an object, it must include a "parameter".`)
			if (parameter.positive !== true && parameter.positive !== false)
				throw new Error(`Invalid Parameter: for a parameter-type expression, if the contents are an object, it must include a "positive" parameter that is either true or false.`)
		}
	}

	static checkExpression(expression) {
		if (expression === undefined || expression === null)
			throw new Error(`Invalid Expression: the given expression did not exist.`)
		if (expression.constructor !== Expression)
			throw new Error(`Invalid Expression: the given expression was not of type "Expression".`)
	}

	// Expression navigation/browsing functions.

	getChildren() {
		switch (this.type) {
			case 'number':
				return []

			case 'parameter':
				return []

			case 'integral':
				return [this.integrand]

			case 'sum':
				return this.terms

			case 'product':
				return this.terms

			case 'fraction':
				return [this.numerator, this.denominator]

			case 'exponent':
				return [this.base]

			case 'ln':
				return [this.contents]

			default:
				throw new Error(`Invalid Expression type "${this.type}".`)
		}
	}

	hasOnlyTypes(types) {
		return this.getChildren().reduce((result, child) => result && child.hasOnlyTypes(types), types.includes(this.type))
	}

	findFirstChildOfType(type) {
		let children = [this]
		while (children.length > 0) {
			children = children.map(child => child.getChildren()).flat()
			const candidate = children.find(child => child.type === type)
			if (candidate)
				return candidate
		}
		return undefined
	}

	findAllChildrenOfType(type) {
		let result = []
		let children = [this]
		while (children.length > 0) {
			children = children.map(child => child.getChildren()).flat()
			result.push(children.filter(child => child.type === type))
		}
		return result.flat()
	}

	isBasicSum() { // A basic sum only has parameters [x1] and numbers.
		return this.type === 'sum' && this.terms.reduce((result, term) => result && term.hasOnlyTypes(['number', 'parameter']), true)
	}

	isBasicSumExponent() { // A basic sum-exponent is an exponent of a basic sum. So like (x1 + 3)^2.
		if (this.isBasicSum())
			return true
		return this.type === 'exponent' && this.base.isBasicSum()
	}

	isBasicSumExponentProduct() { // A basic sum-exponent-product is the product of basic sum-exponents. So like (x1 + 3)^2*(x2 + x3)^3
		if (this.isBasicSumExponent())
			return true
		return this.type === 'product' && this.terms.reduce((result, term) => result && term.isBasicSumExponent(), true)
	}

	// Mathematical solving/simplification functions.

	simplify() {
		let newTerms, incorporated // Some variables used by multiple blocks.
		switch (this.type) {
			case 'product':
				// First simplify the individual terms.
				this.terms.forEach(term => term.simplify())

				// If there are products inside the product, then that's pointless. Expand it.
				newTerms = []
				this.terms.forEach(term => {
					if (term.type === 'product')
						newTerms.push(...term.terms)
					else
						newTerms.push(term)
				})
				this.terms = newTerms

				// Next, start collecting similar terms in the multiplication.
				newTerms = [] // We will put all the resulting terms in here.
				incorporated = this.terms.map(term => false) // An array to check if we have incorporated all the terms.

				// First we gather all the constant factors.
				let factor = 1
				this.terms.forEach((term, i) => {
					if (term.type === 'number') {
						factor *= term.number
						incorporated[i] = true
					}
				})
				if (factor === 0) {
					this.become(new Expression('number', 0))
					break // We have a factor 0 in the product. The whole thing becomes zero.
				}
				if (factor !== 1)
					newTerms.push(new Expression('number', factor))

				// Next, gather all exponents of parameters.
				const parameterPowers = {}
				this.terms.forEach((term, i) => {
					let parameter, power
					if (term.type === 'parameter') {
						parameter = term.parameter
						power = 1
					} else if (term.type === 'exponent' && term.base.type === 'parameter') {
						parameter = term.base.parameter
						power = term.power
					}
					if (parameter) {
						incorporated[i] = true
						parameterPowers[parameter] = (parameterPowers[parameter] || 0) + power
					}
				})
				Object.keys(parameterPowers).forEach(parameter => {
					if (parameterPowers[parameter] === 0)
						return // Don't include powers of zero. They cancel out.
					newTerms.push(new Expression('exponent', {
						base: new Expression('parameter', parameter),
						power: parameterPowers[parameter],
					}))
				})

				// TODO: Merge all more complicated terms together.

				// Incorporate all non-incorporated terms. We don't want to lose anything.
				this.terms.forEach((term, i) => {
					if (!incorporated[i])
						newTerms.push(term)
				})
				this.terms = newTerms

				// The next step is to simplify the signs. We walk through all the elements to see if we need to flip the signs.
				let sign = 1
				this.terms.forEach(term => {
					switch (term.type) {
						case 'number':
							return // Ignore numbers. Only the first element can be one anyway.

						case 'parameter':
							if (!term.positive) {
								sign *= -1
								term.positive = true
							}
							return

						case 'sum':
							return // We don't fix sums yet. Usually they're complicated.

						case 'product':
							throw new Error(`Invalid simplified product: there is a product inside a simplified product, which should not be the case. This indicates an error in the Expression class itself.`)

						case 'exponent':
							if (term.base.type === 'parameter') {
								if (!term.base.positive) {
									sign *= (-1) ** term.power
									term.base.positive = true
								}
							} else if (term.base.type === 'sum') {
								term.base.terms.sort(sumSorting) // Sort the parameters in the sum according to a predefined schedule.
								if (term.base.terms[0].type !== 'parameter')
									throw new Error(`Unexpected sum simplification case: there was a sum to be simplified which didn't start with a parameter. The sum was: "${term.base.toString()}"`)
								if (!term.base.terms[0].positive) { // Should we flip the sign in this sum?
									term.base.terms.forEach(term => term.addMinusSign())
									sign *= (-1) ** term.power
								}
							}
							return

						default:
							return
					}
				})
				if (sign !== 1) {
					if (this.terms[0].type === 'number')
						this.terms[0].number *= sign // Multiply the constant factor by the sign.
					else
						this.terms.unshift(new Expression('number', sign)) // Add a factor "-1" at the start of the array.
				}

				// All done for this case!
				break

			case 'sum':
				// First simplify the individual terms.
				this.terms.forEach(term => term.simplify())

				// If there are sums inside the sum, then that's pointless. Expand it.
				newTerms = []
				this.terms.forEach(term => {
					if (term.type === 'sum')
						newTerms.push(...term)
					else
						newTerms.push(term)
				})
				this.terms = newTerms

				// Check if there are terms in the sum with a denominator (that is, a term with a negative exponent).
				// ToDo: if there is a term in the sum with a negative power, merge the fractions.
				// TODO NEXT: CONTINUE HERE.
				console.log('ToDo: if there is a sum where terms have negative exponents, merge them all together in one big fraction. Figure out how to efficiently check what needs to be added, expand brackets and such.')

				// Next, simplify the sum.
				newTerms = [] // We will put all the resulting terms in here.
				incorporated = this.terms.map(term => false) // An array to check if we have incorporated all the terms.

				// First we gather all the constant factors.
				let sum = 0
				this.terms.forEach((term, i) => {
					if (term.type === 'number') {
						sum += term.number
						incorporated[i] = true
					}
				})
				if (sum !== 0)
					newTerms.push(new Expression('number', sum))

				// Finally, incorporate all non-incorporated terms.
				this.terms.forEach((term, i) => {
					if (!incorporated[i])
						newTerms.push(term)
				})

				// Turn the final result into a sum.
				if (newTerms.length === 0)
					this.become(new Expression('number', 0))
				else if (newTerms.length === 1)
					this.become(newTerms[0])
				else
					this.terms = newTerms
				break

			case 'exponent':
				// First simplify the base.
				this.base.simplify()

				// If the power is 0, become the number "1".
				if (this.power === 0) {
					this.become(new Expression('number', 1))
					break
				}

				// If the exponent is 1, just become the base.
				if (this.power === 1) {
					this.become(this.base)
					this.simplify()
					break
				}

				// If the base is a number, just calculate it.
				if (this.base.type === 'number') {
					this.become(new Expression('number', this.base.number ** this.power))
					break
				}

				// Nothing more can be done.
				break

			default:
				break
		}

		// Return itself to allow chaining.
		return this
	}

	applyPFE(parameter) {
		// Check the expression type.
		if (this.type !== 'product')
			throw new Error(`Invalid PFE request: cannot take a Partial Fraction Expansion for a term that is not a product.`)

		// Make sure we have a simplified expression.
		this.simplify()

		// Browse over all terms that have a negative exponent.
		let pfeTerms = [] // This will be the terms of the resulting sum.
		const terms = this.terms
		terms.forEach((term, termIndex) => {
			// Check if we need to do anything with this term.
			if (term.type !== 'exponent')
				return // Ignore non-exponents, because they are never in the denominator. (We need a negative power for the term to be in the denominator.)
			if (term.power >= 0)
				return // Ignore terms in the numerator.
			if (term.base.type === 'parameter' && term.base.parameter !== parameter)
				return // Ignore y^(-3) if we're doing a PFE over x.
			if (term.base.type === 'sum' && term.base.terms.reduce((result, sumTerm) => result && (sumTerm.type !== 'parameter' || sumTerm.parameter !== parameter), true))
				return // Ignore (y + z + 3)^(-3) if we're doing a PFE over x. So ignore it if all terms in the sum are unequal to the given parameter.

			// We have a term of the form (x + [something])^(-power). First find the substitution "-[something]".
			if (term.base.type !== 'sum')
				throw new Error(`PFE error: we have a term in the denominator which does not have a sum as its base.`)
			if (term.base.terms.length !== 2)
				throw new Error(`PFE error: we have a term in the denominator which does not have two elements in its sum.`)
			const element = term.base.terms.findIndex(sumTerm => (sumTerm.type === 'parameter' && sumTerm.parameter === parameter))
			if (element === -1)
				throw new Error(`PFE error: could not find the PFE-parameter ${parameter} in the term "${term.base.toString()}".`)
			const substitution = term.base.terms[1 - element].clone() // Take the other element. Make sure to clone it, to prevent accidentally changing the original.
			if (term.base.terms[element].positive)
				substitution.addMinusSign() // If we had (x + [something]), add a minus sign. Don't do so if it was (-x + [something]).

			// Next, apply the substitution to find the PFE terms.
			console.log('- Doing stuff with ' + term.toString())
			let productWithoutTerm = new Expression('product', terms.filter((_, index) => index !== termIndex)) // Copy the terms array and cut out the term we're focusing on now for the PFE.
			for (let i = 0; i < -term.power; i++) {
				console.log('Taking the term ' + productWithoutTerm.toString())
				// If needed, take the derivative of this term.
				if (i > 0) {
					console.log('Taking derivative ... ')
					productWithoutTerm = productWithoutTerm.getDerivative(parameter)
					console.log('Derivative is: ' + productWithoutTerm.toString())
				}

				// Insert the right value for the parameter.
				console.log('Substituting ' + parameter.toString() + ' for ' + substitution.toString())
				const coefficient = productWithoutTerm.clone().substitute(parameter, substitution)
				console.log('Coefficient is: ' + coefficient.toString())

				// Set up the relevant term. For this, multiply the coefficient with the relevant term. Add this expression to the PFE sum.
				const power = term.power + i
				const exponent = new Expression('exponent', {
					base: term.base.clone(),
					power: power,
				})
				pfeTerms.push(new Expression('product', [coefficient, exponent]).simplify())
			}
		})

		// Replace this expression by the resulting PFE.
		this.type = 'sum'
		this.terms = pfeTerms
	}

	addMinusSign() {
		switch (this.type) {
			case 'number':
				this.number = -this.number
				break

			case 'parameter':
				this.positive = !this.positive
				break

			default:
				throw new Error(`Invalid addMinusSign call: cannot add a minus sign to an expression of type "${this.type}".`)
		}
	}

	// Turns an expression into its derivative with respect to the given parameter.
	getDerivative(parameter) {
		// Check input.
		if (parameter === undefined)
			throw new Error(`Invalid derivative parameter: the getDerivative function was called without a parameter to take the derivative with respect to.`)

		// Take the derivative. The exact method depends on the type of the expression.
		switch (this.type) {
			case 'number':
				return new Expression('number', 0)

			case 'parameter':
				if (this.parameter !== parameter)
					return new Expression('number', 0) // It's a different parameter.
				if (this.positive)
					return new Expression('number', 1)
				return new Expression('number', -1)

			case 'sum':
				return new Expression('sum', this.terms.map(term => term.getDerivative(parameter)))

			case 'product':
				const sumTerms = []
				const terms = this.terms
				terms.forEach((term, termIndex) => {
					const termsWithoutCurrentTerm = terms.filter((_, index) => index !== termIndex)
					const derivative = term.getDerivative(parameter).simplify()
					if (derivative.type === 'number' && derivative.number === 0)
						return // Don't do anything if the derivative is zero. So if we are considering a term without x.
					termsWithoutCurrentTerm.push(derivative)
					sumTerms.push(new Expression('product', termsWithoutCurrentTerm))
				})
				return new Expression('sum', sumTerms)

			case 'fraction':
				throw new Error(`Invalid call: no derivative of a fraction has been implemented yet.`)

			case 'exponent':
				if (this.power === 1 || this.power === 0) {
					this.simplify()
					return this.getDerivative(parameter)
				}
				return new Expression('product', [
					new Expression('number', this.power),
					this.base.getDerivative(parameter).simplify(),
					new Expression('exponent', {
						base: this.base.clone(),
						power: this.power - 1
					})
				])

			case 'ln':
				return new Expression('product', [
					this.contents.getDerivative(parameter),
					new Expression('exponent', {
						base: this.contents,
						power: -1,
					})
				])

			default:
				throw new Error(`Invalid Expression type "${this.type}" for the getDerivative function.`)
		}
	}

	substitute(parameter, substitution) {
		switch (this.type) {
			case 'number':
				break // A number doesn't change.

			case 'parameter':
				if (this.parameter !== parameter)
					break // It's a different parameter.
				if (this.positive) {
					this.become(substitution.clone())
				} else {
					this.become(substitution.clone())
					this.addMinusSign()
				}
				break

			case 'sum':
			case 'product':
				this.terms.forEach(term => term.substitute(parameter, substitution))
				break

			case 'fraction':
				this.numerator.substitute(parameter, substitution)
				this.denominator.substitute(parameter, substitution)
				break

			case 'exponent':
				this.base.substitute(parameter, substitution)
				break

			case 'ln':
				this.contents.substitute(parameter, substitution)
				break

			default:
				throw new Error(`Invalid Expression type "${this.type}" for the substitute function.`)
		}
		return this // Return itself to allow for chaining.
	}

	integrateOver(parameter) {
		// TODO
	}

	// solveIntegrals(verbose = false) {
	// 	// Let's solve this!
	// 	if (verbose)
	// 		console.log('Trying to solve: ' + this.toString())

	// 	// Resolve all child integrals first.
	// 	let childIntegral
	// 	while (childIntegral = this.findFirstChildOfType('integral')) { // eslint-disable-line no-cond-assign
	// 		if (verbose)
	// 			console.log('Solving inner integral first.')
	// 		childIntegral.solveIntegrals()
	// 	}

	// 	// If this is not an integral, don't do anything more.
	// 	if (this.type !== 'integral')
	// 		return

	// 	// Let's solve the integral.
	// 	this.integrand.mergeToProduct(verbose)
	// 	this.integrand.applyPFE(verbose)

	// 	console.log('Rewritten to ' + this.toString())

	// 	// TODO: SOLVE INTEGRAL.



	// 	// TODO: REMOVE BELOW CLONING PART.
	// 	this.type = this.integrand.type
	// 	switch (this.type) {
	// 		case 'number':
	// 			this.number = this.integrand.number
	// 			break

	// 		case 'parameter':
	// 			this.parameter = this.integrand.parameter
	// 			break

	// 		case 'integral':
	// 			this.integrand = this.integrand.integrand
	// 			this.parameter = this.integrand.parameter
	// 			this.integrand.parent = this
	// 			break

	// 		case 'sum': // Array of [Expression] objects.
	// 		case 'product':
	// 			this.terms = this.integrand.terms
	// 			const parent = this
	// 			this.terms.forEach(term => { term.parent = parent })
	// 			break

	// 		case 'fraction':
	// 			this.numerator = this.integrand.numerator
	// 			this.denominator = this.integrand.denominator
	// 			this.numerator.parent = this
	// 			this.denominator.parent = this
	// 			break

	// 		case 'exponent':
	// 			this.base = this.integrand.base
	// 			this.power = this.integrand.power
	// 			this.base.parent = this
	// 			break

	// 		case 'ln':
	// 			this.contents = this.integrand.contents
	// 			this.contents.parent = this
	// 			break

	// 		default:
	// 			throw new Error(`Invalid Expression type "${this.type}" at cloning.`)
	// 	}
	// }

	// mergeToProduct(verbose) {
	// 	// Check if we have a sum that we should merge to a single product.
	// 	if (this.type !== 'sum')
	// 		return
	// 	if (verbose)
	// 		console.log('Merging the sum to a product...')
	// }

	// isPFEPossible() {
	// 	// Do some basic checks first.
	// 	if (this.type !== 'fraction')
	// 		return false
	// 	if (!this.numerator.hasOnlyTypes(['number', 'parameter', 'sum', 'product', 'exponent']))
	// 		return false
	// 	if (!this.denominator.hasOnlyTypes(['number', 'parameter', 'sum', 'product', 'exponent']))
	// 		return false

	// 	// Run a thorough check on the denominator.
	// 	if (!this.denominator.isBasicSumExponentProduct())
	// 		return false

	// 	// All is in order!
	// 	return true
	// }

	// applyPFE() {
	// 	// Check if a PFE is possible.
	// 	if (!this.isPFEPossible())
	// 		return console.error(`ToDo error: PFE is not possible. We should rewrite the fraction...`)

	// 	// Let's start the PFE process!
	// 	// TODO NEXT: DO THIS. CONTINUE HERE.
	// }

	// Verbose functions.

	print() {
		console.log(this.toString())
	}

	toString() {
		switch (this.type) {
			case 'number':
				return roundTo(this.number, 2)

			case 'parameter':
				if (this.positive)
					return `[${this.parameter}]`
				else
					return `(-[${this.parameter}])`

			case 'integral':
				return `int ${this.integrand.toString()} d[${this.parameter}]`

			case 'sum':
				return `(${this.terms.map(term => term.toString()).join(' + ')})`

			case 'product':
				return this.terms.map(term => term.toString()).join(' * ')

			case 'fraction':
				return `(${this.numerator.toString()}) / (${this.denominator.toString()})`

			case 'exponent':
				const addBrackets = !(this.base.type === 'number' || this.base.type === 'parameter' || this.base.type === 'sum') || (this.base.type === 'number' && this.base.number < 0)
				const base = addBrackets ? `(${this.base.toString()})` : this.base.toString()
				const power = this.power < 0 ? `(${this.power})` : `${this.power}`
				return `${base}^${power}`

			case 'ln':
				return `ln(${this.contents.toString()})`

			default:
				throw new Error(`Invalid expression type "${this.type}".`)
		}
	}
}

const sumSorting = (term1, term2) => {
	if (term1.type === 'number')
		return 1
	if (term2.type === 'number')
		return -1
	if (term1.type === 'parameter' && term2.type === 'parameter') {
		if (term1.parameter === term2.parameter)
			return 0
		else if (term1.parameter < term2.parameter)
			return -1
		else
			return 1
	}
	console.error(`sumSorting problem: an unknown case was encountered in the sum sorting. Term1="${term1.toString()}" and Term2="${term2.toString()}".`)
	return 0
}