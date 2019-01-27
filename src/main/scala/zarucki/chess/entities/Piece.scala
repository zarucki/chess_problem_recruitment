package zarucki.chess.entities

// TODO: should pieces be distinguishable?
object Piece {
	private val allKnownPieces = Set(King(), Queen(), Rook(), Bishop(), Knight())
	private val characterToPieceMap: Map[Char, Piece] = allKnownPieces.map(p => p.representation -> p).toMap

	def apply(letter: Char): Option[Piece] = {
		characterToPieceMap.get(letter)
	}
}

sealed trait Piece {
	def representation: Char

	override final def toString: String = {
		representation.toUpper.toString
	}

	protected def possibleMovesGenerators(address: BoardAddress): List[Stream[MaybeValidBoardAddress]]

	protected def isMoveDestinationWithinBoard(addr: MaybeValidBoardAddress, maxFile: File, maxRank: Int): Boolean = {
		addr.rank <= maxRank && addr.rank >= 0 && addr.fileAsInt <= maxFile.asInt && addr.fileAsInt >= 0
	}

	// TODO: rename this
	// TODO: should this still be a stream? it is not infinite in nature!
	def validMovesFor(startAddress: BoardAddress, maxFile: File, maxRank: Int): Stream[BoardAddress] = {
		possibleMovesGenerators(startAddress)
			.map(readValidBoardAddresses(_, maxFile, maxRank))
  		.reduce(_ ++ _)
	}

	protected def readValidBoardAddresses(moveStream: Stream[MaybeValidBoardAddress], maxFile: File, maxRank: Int): Stream[BoardAddress] = {
		moveStream
			.filter(isMoveDestinationWithinBoard(_, maxFile, maxRank))
			.map(maybeValidBoardAddress => BoardAddress(File(maybeValidBoardAddress.fileAsInt), maybeValidBoardAddress.rank))
	}

	protected type OneStepMovement = MaybeValidBoardAddress => MaybeValidBoardAddress

	protected val toNorth: OneStepMovement = a => a.copy(rank = a.rank + 1)
	protected val toEast: OneStepMovement = a => a.copy(fileAsInt = a.fileAsInt + 1)
	protected val toSouth: OneStepMovement = a => a.copy(rank = a.rank - 1)
	protected val toWest: OneStepMovement = a => a.copy(fileAsInt = a.fileAsInt - 1)

	protected val toNorthEast = toNorth.andThen(toEast)
	protected val toNorthWest = toNorth.andThen(toWest)
	protected val toSouthEast = toSouth.andThen(toEast)
	protected val toSouthWest = toSouth.andThen(toWest)
}

case class King() extends Piece {
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

case class Knight() extends Piece{
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

	protected def continousMovementInDirection(startAddress: BoardAddress, changeFun: OneStepMovement): Stream[MaybeValidBoardAddress] = {
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

case class Queen() extends Piece with MovesManySquares {
	override def representation: Char = 'Q'

	override protected def possibleMovesGenerators(address: BoardAddress): List[Stream[MaybeValidBoardAddress]] = {
		List(
			continousMovementInDirection(address, toNorth),
			continousMovementInDirection(address, toNorthEast),
			continousMovementInDirection(address, toEast),
			continousMovementInDirection(address, toSouthEast),
			continousMovementInDirection(address, toSouth),
			continousMovementInDirection(address, toSouthWest),
			continousMovementInDirection(address, toWest),
			continousMovementInDirection(address, toNorthWest)
		)
	}
}

case class Rook() extends Piece with MovesManySquares {
	override def representation: Char = 'R'

	override protected def possibleMovesGenerators(address: BoardAddress): List[Stream[MaybeValidBoardAddress]] = {
		List(
			continousMovementInDirection(address, toNorth),
			continousMovementInDirection(address, toEast),
			continousMovementInDirection(address, toSouth),
			continousMovementInDirection(address, toWest)
		)
	}
}

case class Bishop() extends Piece with MovesManySquares {
	override def representation: Char = 'B'

	override protected def possibleMovesGenerators(address: BoardAddress): List[Stream[MaybeValidBoardAddress]] = {
		List(
			continousMovementInDirection(address, toNorthEast),
			continousMovementInDirection(address, toSouthEast),
			continousMovementInDirection(address, toSouthWest),
			continousMovementInDirection(address, toNorthWest)
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
