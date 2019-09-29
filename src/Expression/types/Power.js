import { Constant, Expression, Ln, Parameter, Product, Sum } from '..'
import { parseNumber, combination, roundTo } from '../util'

export default class Power extends Expression {
	constructor(base, power = 1, factor = 1) {
		super(factor)
		if (!(base instanceof Expression))
			throw new TypeError(`Invalid base type: an Exponent was made whose base was not an Expression. Instead, the given base was equal to "${base}".`)
		this.base = base
		this.power = parseNumber(power)
		base.parent = this // Set the parent for the child element.
	}

	clone() {
		return new Power(this.base.clone(), this.power, this.factor)
	}

	equals(expression, ignoreFactor = false) {
		if (expression.constructor !== this.constructor)
			return false
		return (ignoreFactor || expression.factor === this.factor) && expression.power === this.power && expression.base.equals(this.base)
	}

	dependsOn(parameter) {
		return this.base.dependsOn(parameter)
	}

	getParameters() {
		return this.base.getParameters()
	}

	substitute(parameter, substitution) {
		return new Power(this.base.substitute(parameter, substitution), this.power, this.factor)
	}

	multiplyBy(multiplication) {
		this.factor *= parseNumber(multiplication)
		return this
	}

	simplify() {
		// If the power is 0, the exponent is 1 and we only have the factor.
		if (this.power === 0)
			return new Constant(this.factor)

		// If the power is 1, become the base.
		if (this.power === 1)
			return this.base.simplify().multiplyBy(this.factor)

		// Okay, stuff is more complicated. First, let's create a clone so we can work safely.
		let result = this.clone()
		result.base = result.base.simplify()

		// If the base is a number, just calculate it. (This must be after the base was simplified.)
		if (result.base instanceof Constant)
			return new Constant(this.factor * (result.base.factor ** this.power))

		// If the base has a factor, pull it out. Then we don't need to worry about it later.
		if (result.base.factor !== 1) {
			result.factor = result.factor * (result.base.factor ** result.power)
			result.base.factor = 1
		}

		// If the base is a power as well, merge the powers. So 3*(2*x^2)*3 becomes 24*x^6.
		if (result.base instanceof Power)
			result = new Power(result.base.base, result.power * result.base.power, result.factor)

		// If the base is a product, expand the brackets. So 2*(3*x^2*y)^3 becomes 54*x^6*y^3.
		if (result.base instanceof Product) {
			const terms = result.base.terms.map(term => new Power(term, result.power).simplify())
			return new Product(terms, result.factor)
		}

		// All done! Return the result 
		return result
	}

	toString(omitMinus = false) {
		const factor = (this.factor === 1 ? `` : `${roundTo(omitMinus ? Math.abs(this.factor) : this.factor, 3)}*`)
		const skipBrackets = (this.base instanceof Constant || this.base instanceof Parameter) && this.base.factor === 1
		const base = skipBrackets ? this.base.toString() : `(${this.base.toString()})`
		const power = this.power < 0 ? `(${this.power})` : `${this.power}`
		return `${factor}${base}^${power}`
	}

	getDerivative(parameter) {
		parameter = this.verifyParameter(parameter)

		// For the simple cases, we should simplify stuff first.
		if (this.power === 1 || this.power === 0)
			return this.simplify().getDerivative(parameter)

		// For the non-simple cases, let's apply the default power derivative rule, including the chain rule.
		return new Product([
			new Power(this.base.clone(), this.power - 1), // Subtract one from the exponent.
			this.base.getDerivative(parameter), // Chain rule: multiply by the derivative of the base.
		], this.factor * this.power).simplify() // Fix the constants too.
	}

	getNumFunctions() {
		return this.base.getNumFunctions() * this.power
	}

	getPolynomialDegree() {
		return this.base.getPolynomialDegree() * this.power
	}

	// expand will expand the brackets of the power. For example, the term (x + 2)^3 will be x^3 + 6*x^2 + 12*x + 8.
	expand() {
		// Check preconditions.
		if (this.power < 0)
			throw new Error(`Cannot expand a power expression with power smaller than zero. The power is "${this.power}".`)
		if (!(this.base instanceof Sum))
			return this // No need to expand.
		if (this.base.terms > 2)
			throw new Error(`Expanding powers of sums with more than two terms is currently not supported.`)

		// Walk through the powers.
		const terms = []
		for (let i = 0; i <= this.power; i++) {
			terms.push(new Product([
				new Power(this.base.terms[0], (this.power - i)),
				new Power(this.base.terms[1], i),
			], combination(this.power, i)))
		}
		return new Sum(terms, this.factor * (this.base.factor ** this.power)).simplify()
	}

	integrateOver(parameter) {
		// Check that this term can be integrated. Only negative powers can be integrated.
		parameter = this.verifyParameter(parameter)
		if (this.power >= 0)
			throw new Error(`It is not possible to integrate over an expression with a non-negative power. The expression you tried to integrate is "${this.toString()}".`)

		// What power do we have?
		let result
		if (this.power <= -2) // We have something like "4/(x+c)^3".
			result = new Power(this.base.clone(), this.power + 1, this.factor * (-1 / (this.power + 1))) // Apply the integral. The minus sign is because we insert the bounds, the infinity bound drops out and the zero bound will get a minus sign added to it.
		else // We have something like 1/(x+c). The power is -1.
			result = new Ln(this.base.clone(), -this.factor) // Again, the minus sign is because we apply the integration bounds.
		result = result.substitute(parameter, new Constant(0)) // Plugging in the bound.
		return result.simplify()
	}
}