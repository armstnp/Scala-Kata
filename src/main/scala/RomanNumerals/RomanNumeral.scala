package RomanNumerals

import scala.language.postfixOps

object RomanNumeral {
	sealed class RomanNumeral private[RomanNumerals](val char: String, val value: Int) {
		def isPrefixOf(romanNumeral: String) = romanNumeral startsWith char
	}

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

	implicit class RomanNumeralWrapper(val romanNumeral: String) {
		def toArabic = RomanNumeral toArabic romanNumeral
	}

	def apply(num: Int): String = {
		require(num > 0, "Roman numerals are only available for positive integers.")
		buildNumeral(num, new StringBuilder)
	}

	def from = apply _

	private def buildNumeral(value: Int, accumulation: StringBuilder): String = {
		val maxNumeral = getMaximallyFilling(value)
		maxNumeral match {
			case Some(numeral) ⇒ buildNumeral(value - numeral.value, accumulation append numeral.char)
			case None ⇒ accumulation mkString
		}
	}

	private def getMaximallyFilling(num: Int) =
		descendingNumerals
				.dropWhile(_.value > num)
				.headOption

	def toArabic(romanNumeral: String) = {
		require(isValidRomanNumeral(romanNumeral), "Only valid roman numerals can be converted to arabic")
		buildArabic(romanNumeral, 0)
	}

	private def isValidRomanNumeral(romanNumeral: String) =
		!(romanNumeral isEmpty) && (romanNumeral matches "M*(CM|CD|(D?C{0,3}))(XC|XL|(L?X{0,3}))(IX|IV|(V?I{0,3}))")

	private def buildArabic(romanNumeral: String, accumulation: Int): Int =
		if(romanNumeral isEmpty) accumulation
		else {
			val matchNumeral = getMatchingNumeral(romanNumeral)
			matchNumeral match {
				case Some(numeral) ⇒ buildArabic(romanNumeral drop numeral.char.length, accumulation + numeral.value)
				case None ⇒ throw new IllegalArgumentException("Only valid roman numerals can be converted to arabic")
			}
		}

	private def getMatchingNumeral(romanNumeral: String) =
		descendingNumerals
				.dropWhile(instance ⇒ !(instance isPrefixOf romanNumeral))
				.headOption
}
