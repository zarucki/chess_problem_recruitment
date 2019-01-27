package zarucki.chess.entities

object PieceHelper {
	private val allKnownPieces = Set(King, Queen, Rook, Bishop, Knight)
	private val characterToPieceMap: Map[Char, Piece] = allKnownPieces.map(p => p.representation -> p).toMap

	def fromLetter(letter: Char): Option[Piece] = {
		characterToPieceMap.get(letter)
	}
}

final case object King extends Piece {
	override def representation: Char = 'K'

	override protected def possibleMovesGenerators(address: BoardAddress): List[Stream[MaybeValidBoardAddress]] = {
		val mvba = MaybeValidBoardAddress(address)
		List(
			toNorth(mvba) #:: toNorthEast(mvba) #:: toEast(mvba) #::
			toSouthEast(mvba) #:: toSouth(mvba) #:: toSouthWest(mvba) #::
			toWest(mvba) #:: toNorthWest(mvba) #:: Stream.empty[MaybeValidBoardAddress]
		)
	}
}

final case object Knight extends Piece {
	override def representation: Char = 'N'

	private val cachedComplexMovements: List[OneStepMovement] = List(
			toNorth.andThen(toNorth).andThen(toWest),
			toNorth.andThen(toNorth).andThen(toEast),
			toEast.andThen(toEast).andThen(toNorth),
			toEast.andThen(toEast).andThen(toSouth),
			toSouth.andThen(toSouth).andThen(toEast),
			toSouth.andThen(toSouth).andThen(toWest),
			toWest.andThen(toWest).andThen(toSouth),
			toWest.andThen(toWest).andThen(toNorth)
		)

	override protected def possibleMovesGenerators(address: BoardAddress): List[Stream[MaybeValidBoardAddress]] = {
		val mvba = MaybeValidBoardAddress(address)
		List(cachedComplexMovements.map(_.apply(mvba)).toStream)
	}
}

// TODO: not exactly sure this is most clear way to do this
trait MovesManySquares {
	self: Piece =>

	protected def continuousMovementInDirection(startAddress: BoardAddress, changeFun: OneStepMovement): Stream[MaybeValidBoardAddress] = {
		lazy val moveStream: Stream[MaybeValidBoardAddress] = changeFun(MaybeValidBoardAddress(startAddress)) #:: moveStream.map(addr => changeFun(addr))
		moveStream
	}

	// Streams for continous pieces are infinite, we need to read them til the first invalid move
	override protected  def readValidBoardAddresses(moveStream: scala.Stream[MaybeValidBoardAddress], maxFile: File, maxRank: Int): scala.Stream[BoardAddress] = {
		moveStream
			.takeWhile(isMoveDestinationWithinBoard(_, maxFile, maxRank))
			.map(maybeValidBoardAddress => BoardAddress(File(maybeValidBoardAddress.fileAsInt), maybeValidBoardAddress.rank))
	}
}

final case object Queen extends Piece with MovesManySquares {
	override def representation: Char = 'Q'

	override protected def possibleMovesGenerators(address: BoardAddress): List[Stream[MaybeValidBoardAddress]] = {
		List(
			continuousMovementInDirection(address, toNorth),
			continuousMovementInDirection(address, toNorthEast),
			continuousMovementInDirection(address, toEast),
			continuousMovementInDirection(address, toSouthEast),
			continuousMovementInDirection(address, toSouth),
			continuousMovementInDirection(address, toSouthWest),
			continuousMovementInDirection(address, toWest),
			continuousMovementInDirection(address, toNorthWest)
		)
	}
}

final case object Rook extends Piece with MovesManySquares {
	override def representation: Char = 'R'

	override protected def possibleMovesGenerators(address: BoardAddress): List[Stream[MaybeValidBoardAddress]] = {
		List(
			continuousMovementInDirection(address, toNorth),
			continuousMovementInDirection(address, toEast),
			continuousMovementInDirection(address, toSouth),
			continuousMovementInDirection(address, toWest)
		)
	}
}

final case object Bishop extends Piece with MovesManySquares {
	override def representation: Char = 'B'

	override protected def possibleMovesGenerators(address: BoardAddress): List[Stream[MaybeValidBoardAddress]] = {
		List(
			continuousMovementInDirection(address, toNorthEast),
			continuousMovementInDirection(address, toSouthEast),
			continuousMovementInDirection(address, toSouthWest),
			continuousMovementInDirection(address, toNorthWest)
		)
	}
}

object MaybeValidBoardAddress {
	def apply(boardAddress: BoardAddress): MaybeValidBoardAddress = {
		MaybeValidBoardAddress(fileAsInt = boardAddress.file.asInt, rank = boardAddress.rank)
	}
}

// TODO: maybe instead of this introduce some NIL to BoardAddress
case class MaybeValidBoardAddress(fileAsInt: Int, rank: Int)
