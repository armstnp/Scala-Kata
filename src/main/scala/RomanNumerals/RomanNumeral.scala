package RomanNumerals

object RomanNumeral {
	sealed class RomanNumeral private[RomanNumerals](val char: String, val value: Int)
	private val I = new RomanNumeral("I", 1)
	private val IV = new RomanNumeral("IV", 4)
	private val V = new RomanNumeral("V", 5)
	private val IX = new RomanNumeral("IX", 9)
	private val X = new RomanNumeral("X", 10)
	private val XL = new RomanNumeral("XL", 40)
	private val L = new RomanNumeral("L", 50)
	private val XC = new RomanNumeral("XC", 90)
	private val C = new RomanNumeral("C", 100)
	private val CD = new RomanNumeral("CD", 400)
	private val D = new RomanNumeral("D", 500)
	private val CM = new RomanNumeral("CM", 900)
	private val M = new RomanNumeral("M", 1000)

	private val descendingNumerals = Seq(M, CM, D, CD, C, XC, L, XL, X, IX, V, IV, I)

	def apply(num: Int): String = {
		require(num > 0, "Roman numerals are only available for positive integers.")
		buildNumeral(num, new StringBuilder)
	}

	def from = apply _

	private def buildNumeral(value: Int, accumulation: StringBuilder): String = {
		val maxNumeral = getMaximallyFilling(value)
		getMaximallyFilling(value) match {
			case Some(numeral) ⇒ buildNumeral(value - numeral.value, accumulation append numeral.char)
			case None ⇒ accumulation mkString
		}
	}

	private def getMaximallyFilling(num: Int) =
		descendingNumerals
				.dropWhile(_.value > num)
				.headOption
}
