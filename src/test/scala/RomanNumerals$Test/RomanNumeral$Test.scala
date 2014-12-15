package RomanNumerals$Test

import scala.language.postfixOps
import RomanNumerals._
import RomanNumerals.RomanNumeral._
import org.scalatest._

object RomanNumeralTestHelper {
	implicit class IntToRomanNumeralComparison(private val value: Int) extends AnyVal {
		def isRepresentedBy(romanNumeral: String) = (RomanNumeral from value) == romanNumeral
	}

	implicit class RomanNumeralToIntComparison(private val romanNumeral: String) extends AnyVal {
		def represents(value: Int) = (romanNumeral toArabic) == value
	}
}
import RomanNumeralTestHelper._

class RomanNumeral$Test extends FlatSpec {
	"RomanNumeral" should "throw an IllegalArgumentException when the roman numeral for numbers < 1 are requested" in {
		intercept[IllegalArgumentException] {
			RomanNumeral from 0
		}

		intercept[IllegalArgumentException] {
			RomanNumeral from -1
		}
	}

	it must "give roman numeral representations of all the atomic roman numeral arabic values (e.g. 1, 5, 10, etc.)"  in
	{
		assert(1 isRepresentedBy "I")
		assert(5 isRepresentedBy "V")
		assert(10 isRepresentedBy "X")
		assert(50 isRepresentedBy "L")
		assert(100 isRepresentedBy "C")
		assert(500 isRepresentedBy "D")
		assert(1000 isRepresentedBy "M")
	}

	it must "give roman numeral representations for purely additive values (e.g. 2, 3, 6, 7, 8, 11, etc.)" in {
		assert(2 isRepresentedBy "II")
		assert(3 isRepresentedBy "III")
		assert(6 isRepresentedBy "VI")
		assert(1666 isRepresentedBy "MDCLXVI")
	}

	it must "give roman numeral representations for values with subtractive components (e.g. 4, 9, 14, 19, 40, etc.)" in
	{
		assert(4 isRepresentedBy "IV")
		assert(9 isRepresentedBy "IX")
		assert(14 isRepresentedBy "XIV")
		assert(999 isRepresentedBy "CMXCIX")
	}

	it should "throw an IllegalArgumentException when the arabic representation of an empty roman numeral is " +
			  "requested" in {
		intercept[IllegalArgumentException] {
			"" represents -1
		}
	}

	it should "throw an IllegalArgumentException when the arabic representation of an roman numeral containing " +
	          "invalid characters is requested" in {
		intercept[IllegalArgumentException]{
			"Z" represents -1
		}
		intercept[IllegalArgumentException]{
			"MDCLXVIZ" represents -1
		}
	}

	it should "throw an IllegalArgumentException when the arabic representation of a roman numeral containing " +
	          "invalid sequences is requested" in {
		intercept[IllegalArgumentException] {
			"IIII" represents 4
		}

		intercept[IllegalArgumentException]{
			"MCDC" represents 1500
		}
	}


	it must "give arabic values of all the atomic roman numerals (e.g. I, V, X, etc.)" in {
		assert("I" represents 1)
		assert("V" represents 5)
		assert("X" represents 10)
		assert("L" represents 50)
		assert("C" represents 100)
		assert("D" represents 500)
		assert("M" represents 1000)
	}

	it must "give arabic values for purely additive roman numerals (e.g. II, III, VI, VII, etc.)" in {
		assert("II" represents 2)
		assert("III" represents 3)
		assert("VI" represents 6)
		assert("MDCLXVI" represents 1666)
	}

	it must "give arabic values for roman numerals containing subtractive  components" +
			"(e.g. IV, IX, XIV, XIX, XL, etc.)" in {
		assert("IV" represents 4)
		assert("IX" represents 9)
		assert("XIV" represents 14)
		assert("CMXCIX" represents 999)
	}
}
