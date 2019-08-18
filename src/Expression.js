import { roundTo } from './util.js'

export default class Expression {
	constructor(type, contents, parent) {
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
				if (!Number.isInteger(power) || power < 1)
					throw new Error(`Invalid Expression: an exponent-type expression must have a positive integer number as its power.`)
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

	// Check functions.

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

	solveIntegrals() {
		// Resolve all child integrals first.
		let childIntegral
		while (childIntegral = this.findFirstChildOfType('integral')) { // eslint-disable-line no-cond-assign
			childIntegral.solveIntegrals()
		}

		// If this is not an integral, don't do anything more.
		if (this.type !== 'integral')
			return

		// Let's solve the integral.
		console.log('Solving ' + this.toString())
		this.integrand.applyPFE()
		console.log('Rewritten to ' + this.toString())

		// TODO: SOLVE INTEGRAL.



		// TODO: REMOVE BELOW CLONING PART.
		this.type = this.integrand.type
		switch (this.type) {
			case 'number':
				this.number = this.integrand.number
				break

			case 'parameter':
				this.parameter = this.integrand.parameter
				break

			case 'integral':
				this.integrand = this.integrand.integrand
				this.parameter = this.integrand.parameter
				this.integrand.parent = this
				break

			case 'sum': // Array of [Expression] objects.
			case 'product':
				this.terms = this.integrand.terms
				const parent = this
				this.terms.forEach(term => { term.parent = parent })
				break

			case 'fraction':
				this.numerator = this.integrand.numerator
				this.denominator = this.integrand.denominator
				this.numerator.parent = this
				this.denominator.parent = this
				break

			case 'exponent':
				this.base = this.integrand.base
				this.power = this.integrand.power
				this.base.parent = this
				break

			case 'ln':
				this.contents = this.integrand.contents
				this.contents.parent = this
				break

			default:
				throw new Error(`Invalid Expression type "${this.type}" at cloning.`)
		}
	}

	isPFEPossible() {
		// Do some basic checks first.
		if (this.type !== 'fraction')
			return false
		if (!this.numerator.hasOnlyTypes(['number', 'parameter', 'sum', 'product', 'exponent']))
			return false
		if (!this.denominator.hasOnlyTypes(['number', 'parameter', 'sum', 'product', 'exponent']))
			return false

		// Run a thorough check on the denominator.
		if (!this.denominator.isBasicSumExponentProduct())
			return false

		// All is in order!
		return true
	}

	applyPFE() {
		// Check if a PFE is possible.
		if (!this.isPFEPossible())
			return console.error(`ToDo error: PFE is not possible. We should rewrite the fraction...`)

		// Let's start the PFE process!
		// TODO NEXT: DO THIS. CONTINUE HERE.
	}

	// Verbose functions.

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