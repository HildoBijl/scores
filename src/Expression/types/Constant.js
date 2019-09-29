import { Expression } from '..'
import { roundTo } from '../util'

export default class Constant extends Expression {
	constructor(factor = 0) {
		super(factor)
	}

	clone() {
		return new Constant(this.factor)
	}

	equals(expression, ignoreFactor = false) {
		if (expression.constructor !== this.constructor)
			return false
		return ignoreFactor || expression.factor === this.factor
	}

	dependsOn() {
		return false
	}

	getParameters() {
		return []
	}
	
	substitute() {
		return this.clone() // A constant does not change upon substitution.
	}

	simplify() {
		return this // You cannot simplify a number. It's as simple as it gets.
	}

	toString(omitMinus = false) {
		return `${roundTo(omitMinus ? Math.abs(this.factor) : this.factor, 3)}`
	}

	getDerivative() {
		return new Constant(0)
	}

	getNumFunctions() {
		return 0
	}

	getPolynomialDegree() {
		return 0
	}
}