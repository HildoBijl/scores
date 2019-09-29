// This is the abstract FunctionExpression class. It should not be instantiated, but it is used for Ln, Sin, Cos and such.

import { FunctionExpression, Power, Product, Constant } from '..'

export default class Ln extends FunctionExpression {
	simplify() {
		// First simplify the argument.
		const argument = this.argument.simplify()

		// If there is a constant value, calculate it.
		if (argument instanceof Constant)
			return new Constant(Math.log(argument.factor))

		// If there is a power inside, extract the power into a factor. Turn 4*ln(2*(x+1)^3) into 12*ln(2^(1/3)*(x+1)).
		if (argument instanceof Power) {
			const newArgument = argument.base.clone()
			newArgument.factor *= argument.factor ** (1 / argument.power)
			return new Ln(newArgument, this.factor * argument.power)
		}

		// No significant changes can be made, apart from what already happened to the argument.
		return new Ln(argument, this.factor)
	}

	getDerivative(parameter) {
		// Turn c*ln(f(x)) into c*f'(x) / f(x).
		parameter = this.verifyParameter(parameter)
		return new Product([
			this.argument.getDerivative(parameter),
			new Power(this.argument.simplify(), -1),
		], this.factor).simplify()
	}
}