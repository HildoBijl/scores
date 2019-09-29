// parseNumber takes an input parameter and either turns it into a number or throws an error if this does not seem to work.
export default function parseNumber(number) {
	const numberAsFloat = parseFloat(number)
	if (isNaN(numberAsFloat) || !isFinite(number))
		throw new TypeError(`A number was assumed, but the given value was "${number}".`)
	return numberAsFloat
}