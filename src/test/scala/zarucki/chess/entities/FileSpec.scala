package zarucki.chess.entities

import zarucki.chess.UnitSpec

class FileSpec extends UnitSpec {
	behavior of "chess File creation and validation"

	it should "it should uppercase letters" in {
		assert(File("a").toString == "A")
	}

	it should "be case insensitive" in {
		assert(File("a") == File("A"))
	}

	it should "be equal even with other constructor" in {
		assert(File("a") == File(0))
	}

	it should "not allow numbers" in {
		assertThrows[AssertionError](
			File("A1")
		)
	}

	it should "not allow weird native characters" in {
		assertThrows[AssertionError](File("ą"))
		assertThrows[AssertionError](File("ś"))
		assertThrows[AssertionError](File("ń"))
		assertThrows[AssertionError](File("ż"))
		assertThrows[AssertionError](File("ł"))
		assertThrows[AssertionError](File("ó"))
	}

	behavior of "chess File to column index conversion"

	it should "correctly handle that A is zero" in {
		assert(File("a").asInt == 0)
		assert(File("aa").asInt == 0)
		assert(File("aaa").asInt == 0)
		assert(File("aab").asInt == 1)
		assert(File("abb").asInt == 1 * 26 + 1)

		assert(File(0).asString == "A")
	}

	it should "correctly convert from chess File to Int" in {
		assert(File("a").asInt == 0)
		assert(File("b").asInt == 1)
		assert(File("c").asInt == 2)
		assert(File("d").asInt == 3)
		assert(File("x").asInt == 23)
		assert(File("y").asInt == 24)
		assert(File("z").asInt == 25)

		assert(File("ba").asInt == 1 * 26 + 0)
		assert(File("bb").asInt == 1 * 26 + 1)
		assert(File("bc").asInt == 1 * 26 + 2)
		assert(File("bx").asInt == 1 * 26 + 23)
		assert(File("by").asInt == 1 * 26 + 24)
		assert(File("bz").asInt == 1 * 26 + 25)
		assert(File("ca").asInt == 2 * 26)
		assert(File("zz").asInt == 25 * 26 + 25)
		assert(File("baa").asInt == 1 * 26 * 26)

		assert(File("zzz").asInt == 25 * (26*26) + 25 * 26 + 25)
		assert(File("baaa").asInt == 26*26*26)

		assert(File("baa").asInt - File("zz").asInt == 1)
		assert(File("baaa").asInt - File("zzz").asInt == 1)
	}

	it should "correctly convert from int to chess File" in {
		assert(File(0).asString == "A")
		assert(File(3).asString == "D")
		assert(File(4).asString == "E")
		assert(File(20).asString == "U")
		assert(File(21).asString == "V")
		assert(File(25).asString == "Z")
		assert(File(26).asString == "BA")
		assert(File(29).asString == "BD")
		assert(File(30).asString == "BE")
		assert(File(46).asString == "BU")
		assert(File(47).asString == "BV")
		assert(File(51).asString == "BZ")
		assert(File(39).asString == "BN")
		assert(File(729).asString == "BCB")
		assert(File(26*26*26).asString == "BAAA")
		assert(File(18277).asString == "BBAZ")
	}
}
