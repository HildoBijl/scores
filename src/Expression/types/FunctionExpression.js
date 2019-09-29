// This is the abstract FunctionExpression class. It should not be instantiated, but it is used for Ln, Sin, Cos and such.

import { Expression } from '..'
import { roundTo } from '../util'

export default class FunctionExpression extends Expression {
	constructor(argument, factor = 1) {
		// Check and store input.
		super(factor)
		if (!(argument instanceof Expression))
			throw new TypeError(`Invalid term: tried to create a ${this.constructor.name}-function, but the argument given was not an Expression. Its value was "${argument}".`)
		this.argument = argument
		argument.parent = this
	}

	equals(expression, ignoreFactor = false) {
		if (expression.constructor !== this.constructor)
			return false
		if (!ignoreFactor && expression.factor !== this.factor)
			return false
		return this.argument.equals(expression.argument)
	}

	dependsOn(parameter) {
		return this.argument.dependsOn(parameter)
	}

	getParameters() {
		return this.argument.getParameters()
	}
	
	substitute(parameter, substitution) {
		return new this.constructor(this.argument.substitute(parameter, substitution), this.factor)
	}

	clone(deep = true) {
		return new this.constructor(deep ? this.argument.clone(deep) : this.argument, this.factor)
	}

	toString(omitMinus = false) {
		return (this.factor === 1 ? `` : `${roundTo(omitMinus ? Math.abs(this.factor) : this.factor, 3)}*`) + 
			this.getType().toLowerCase() + 
			`(${this.argument.toString()})`
	}

	getNumFunctions() {
		return 1 + this.argument.getNumFunctions() // Count itself, plus whatever is inside.
	}

	getPolynomialDegree() {
		return this.argument.getPolynomialDegree() // As polynomial degree, we ignore this function and only look at the argument. This isn't really all too important; it's used for sorting the terms in sums/products.
	}
}