// parseInt takes an input parameter and either turns it into a number or throws an error if this does not seem to work.
export default function parseInteger(number) {
	const numberAsInt = parseInt(number)
	if (!Number.isInteger(numberAsInt))
		throw new TypeError(`An integer number was assumed, but the given value was "${number}".`)
	if (numberAsInt != number) // eslint-disable-line eqeqeq
		throw new TypeError(`An integer number was assumed, but the given value was "${number}".`)
	return numberAsInt
}