// This is the abstract Expression class. It should not be instantiated itself, but child classes should implement it.

import { parseNumber } from '../util'

export default class Expression {
	constructor(factor = 1) {
		// This class may not be instantiated.
		if (this.constructor === Expression)
			throw new TypeError(`Abstract class "Expression" may not be instantiated directly.`)

		// Certain methods must be implemented in child classes.
		const methods = ['clone', 'equals', 'dependsOn', 'getParameters', 'substitute', 'simplify', 'toString', 'getDerivative', 'getNumFunctions']
		methods.forEach(method => {
			if (this[method] === undefined || this[method] === Object.prototype[method]) // The Object object has some default methods, and those are not acceptable either.
				throw new Error(`Child classes of the Expression class must implement the "${method}" method.`)
		})

		// Store the factor, which every Expression has.
		this.factor = parseNumber(factor)
	}

	// print will log a string representation of this expression.
	print() {
		console.log(this.toString())
	}

	// getType returns the type of an expression. This is the name of the constructor.
	getType() {
		return this.constructor.name
	}

	// multiplyBy create a clone of this expression which is multiplied by the given number. So multiplying "2*x + y" by 3 gives "3*(2*x + y)". The original object is unchanged.
	multiplyBy(multiplication) {
		const result = this.clone()
		result.factor*= parseNumber(multiplication)
		return result
	}

	// eliminateFactor creates a clone of this expression, but then with a factor of 1 (default).
	eliminateFactor() {
		return this.multiplyBy(1/this.factor)
	}

	// verifyParameter is used by functions requiring a parameter as input. It checks the given parameter. If no parameter is given, it tries to figure out which parameter was meant.
	verifyParameter(parameter) {
		// If no parameter was given, try to find one.
		if (parameter === undefined) {
			const parameters = this.getParameters()
			if (parameters.length === 0)
				parameter = 'x' // Default.
			else if (parameters.length > 1)
				throw new TypeError(`No parameter was given. Also, the given expression depends on multiple parameters, so no default parameter could be extracted. The expression is "${this.toString()}".`)
			else
				parameter = parameters[0] // If the expression only depends on one parameter, just assume that one was meant.
		}

		// Check that the parameter is a string.
		if (typeof parameter !== 'string')
			throw new TypeError(`The given parameter was not a string. Instead, it equals "${parameter}".`)

		// All is in order. Return the parameter.
		return parameter
	}

	// integrateOver tries to evaluate the integral over the expression, with respect to the given parameter, from 0 to infinity.
	integrateOver(parameter) {
		throw new TypeError(`Integrating over an expression of type "${this.getType()}" is not supported yet.`)
	}

	/* The following functions need to be implemented.
	 * clone(deep = true) creates a new object identical to the given one. If deep is true, all sub-expressions are cloned too, so no double references occur.
	 * equals(expression, ignoreFactor = false) checks equality between two expressions. This equality check is basic. While "1 + x" and "x + 1" will be considered equal, "(x+1)/x" and "1+1/x" will not be considered equal. After all, they are of different type. It is wise to simplify expressions manually before checking for equality. When ignoreFactor is set to true, then 2*x will be seen as equal to x.
	 * dependsOn(parameter) checks whether the expression depends on the given parameter and returns true or false.
	 * getParameters() extracts all parameters inside the expression and returns them as a sorted array of strings. So for the expression y*z^2/(z+x) you get ['x','y','z'].
	 * substitute(parameter, substitution) returns the same expression, except that the given parameter (for instance "x") has been replaced by the given substitution Expression (for instance 2*y^2). The original object is NOT changed; an adjusted clone is returned.
	 * simplify() returns a (potentially new, potentially existing) expression that is a simplified version of the current expression. For example, simplifying the sum 2*x + x will result in the Expression 3*x. The original object is NOT changed; it is either returned unchanged or an adjusted clone is returned.
	 * toString(omitMinus = false) returns a string representation of the object. If omitMinus is set to true, then a minus sign will not be added to the string representation.
	 * getDerivative(parameter) returns a derivative with respect to the given parameter. The derivative is always automatically simplified.
	 * getNumFunctions() counts the number of functions that are inside this Expression. For example, the sum ln(x) + ln(ln(y)) + (ln(y))^2 has five functions.
	 * getPolynomialDegree() returns the highest degree of polynomials. Functions are ignored. So if we have x^2*(y + z^3)^4*ln(x^20) then the degree is 14.
	 */
}
