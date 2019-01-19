package zarucki.chess.entities

import scala.annotation.tailrec

object File {
	def apply(fileAsString: String): File = {
		val normalized = fileAsString.toUpperCase
		new File(zeroBasedColumnNumber = baseToInt(normalized), knownStringRepresentation = Some(normalized))
	}

	def apply(zeroBasedColumnNumber: Int): File = {
		new File(zeroBasedColumnNumber)
	}

	private def intToBase(positiveNumber: Int): String = {
		assert(positiveNumber >= 0, "number was less than 0")

		@tailrec
		def toFileBase(positiveNumber: Int, repr: String = ""): String = {
			if (positiveNumber == 0) {
				repr
			} else {
				toFileBase(positiveNumber / fileBase, baseCharacterSet(positiveNumber % fileBase) + repr)
			}
		}

		val numberAsBase = toFileBase(positiveNumber)
		if (numberAsBase.isEmpty) baseCharacterSet.head.toString else numberAsBase
	}

	private def baseToInt(baseString: String): Int = {
		assert(baseString.nonEmpty, "Empty string is not a valid base string.")
		assert(baseString.forall(baseCharacterSet.contains(_)), s"Not all characters in '$baseString' are in the base charset '$baseCharacterSet'!")

		baseString.view
			.map(baseCharacterSet.indexOf(_))
			.reduceLeft(_ * fileBase + _)
	}

	private val baseCharacterSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	private val fileBase = baseCharacterSet.size
}

/**
	* Represents columns on chess boards, it is a base26 number
	* @param fileAsString
	* @example A,B,C,(...),Z,BA,BB,BC,(...),BZ,CA,CB,CC,(...),ZX,ZY,ZZ,BAA,BAB,BAC,(...)
	*/
class File private (zeroBasedColumnNumber: Int, knownStringRepresentation: Option[String] = None) extends Equals {
	assert(zeroBasedColumnNumber >= 0, "column number was less than 0")

	// TODO: implicit conversion to int?
	lazy val asInt: Int = zeroBasedColumnNumber
	lazy val asString: String = knownStringRepresentation.getOrElse(File.intToBase(zeroBasedColumnNumber))

	override def toString: String = asString

	override def hashCode(): Int = zeroBasedColumnNumber.hashCode()

	override def canEqual(that: Any): Boolean = that.isInstanceOf[File]

	override def equals(that: Any): Boolean = {
		that match {
			case that: File => that.canEqual(this) && this.hashCode == that.hashCode
			case _ => false
		}
	}
}

