import { Constant, Expression } from '..'
import { roundTo } from '../util'

export default class Parameter extends Expression {
	constructor(name = 'x', factor = 1) {
		super(factor)
		this.name = name
	}

	clone() {
		return new Parameter(this.name, this.factor)
	}

	equals(expression, ignoreFactor = false) {
		if (expression.constructor !== this.constructor)
			return false
		return (ignoreFactor || expression.factor === this.factor) && expression.name === this.name
	}

	dependsOn(parameter) {
		const parameterName = parameter instanceof Parameter ? parameter.name : parameter
		return this.name === parameterName
	}

	getParameters() {
		return [this.name]
	}

	substitute(parameter, substitution) {
		// Check input.
		if (!(substitution instanceof Expression))
			throw new TypeError(`Invalid substitution: when substituting, an Expression should be given to substitute with. Instead, the substitution given was "${substitution}".`)
		if (typeof parameter !== 'string')
			throw new TypeError(`Invalid substitution: when substituting, the given "parameter" must be a string. The current given parameter was "${parameter}".`)

		// Check parameter name and apply substitution.
		if (this.name !== parameter)
			return this // It's a different parameter. No change takes place.
		return substitution.multiplyBy(this.factor) // Replace this parameter by a clone of the substitution, multiplied by the current parameter's factor.
	}

	simplify() {
		return this // Parameter type expressions don't get any simpler.
	}

	toString(omitMinus = false) {
		if (this.factor === 1 || (this.factor === -1 && omitMinus))
			return this.name
		if (this.factor === -1)
			return `-${this.name}`
		return `${roundTo(omitMinus ? Math.abs(this.factor) : this.factor, 3)}*${this.name}`
	}

	getDerivative(parameter) {
		parameter = this.verifyParameter(parameter)
		if (this.name !== parameter)
			return new Constant(0) // It's a different parameter.
		return new Constant(this.factor)
	}

	getNumFunctions() {
		return 0
	}

	getPolynomialDegree() {
		return 1
	}
}