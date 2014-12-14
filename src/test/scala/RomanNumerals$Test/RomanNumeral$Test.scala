package RomanNumerals$Test

import RomanNumerals._
import org.scalatest._

object RomanNumeralTestHelper {
	implicit class IntToRomanNumeralComparison(private val value: Int) extends AnyVal {
		def isRepresentedBy(romanNumeral: String) = (RomanNumeral from value) == romanNumeral
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

	it must "give roman numeral representations of all the atomic roman numeral values (e.g. 1, 5, 10, etc.)" in {
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
}
