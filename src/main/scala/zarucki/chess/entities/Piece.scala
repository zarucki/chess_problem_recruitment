package zarucki.chess.entities

import File.file2Integer
// TODO: rename things here
// TODO: it is messy here
trait Piece {
	def representation: Char

	def canMoveFromTo(fromAddress: BoardAddress, toAddress: BoardAddress): Boolean

	// TODO: rename this
	// TODO: should this still be a stream? it is not infinite in nature!
	def validMovesFor(startAddress: BoardAddress, maxFile: File, maxRank: Int): Stream[BoardAddress] = {
		possibleMovesGenerators(startAddress)
			.map(readValidBoardAddresses(_, maxFile, maxRank))
			.reduce(_ ++ _)
	}

	override final def toString: String = {
		representation.toUpper.toString
	}

	protected def possibleMovesGenerators(address: BoardAddress): List[Stream[MaybeValidBoardAddress]]

	protected def isMoveDestinationWithinBoard(addr: MaybeValidBoardAddress, maxFile: File, maxRank: Int): Boolean = {
		addr.rank <= maxRank && addr.rank >= 0 && addr.fileAsInt <= maxFile && addr.fileAsInt >= 0
	}

	protected def readValidBoardAddresses(moveStream: Stream[MaybeValidBoardAddress], maxFile: File, maxRank: Int): Stream[BoardAddress] = {
		moveStream
			.filter(isMoveDestinationWithinBoard(_, maxFile, maxRank))
			.map(maybeValidBoardAddress => BoardAddress(File(maybeValidBoardAddress.fileAsInt), maybeValidBoardAddress.rank))
	}


	protected object MaybeValidBoardAddress {
		def apply(boardAddress: BoardAddress): MaybeValidBoardAddress = {
			MaybeValidBoardAddress(fileAsInt = boardAddress.file, rank = boardAddress.rank)
		}
	}

	protected case class MaybeValidBoardAddress(fileAsInt: Int, rank: Int)

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

// TODO: not exactly sure this is most clear way to do this
trait MovesManySquares {
	self: Piece =>

	protected def continuousMovementInDirection(startAddress: BoardAddress, changeFun: OneStepMovement): Stream[MaybeValidBoardAddress] = {
		lazy val moveStream: Stream[MaybeValidBoardAddress] = changeFun(MaybeValidBoardAddress(startAddress)) #:: moveStream.map(addr => changeFun(addr))
		moveStream
	}

	// Streams for continuous pieces are infinite, we need to read them til the first invalid move
	override protected  def readValidBoardAddresses(moveStream: scala.Stream[MaybeValidBoardAddress], maxFile: File, maxRank: Int): scala.Stream[BoardAddress] = {
		moveStream
			.takeWhile(isMoveDestinationWithinBoard(_, maxFile, maxRank))
			.map(maybeValidBoardAddress => BoardAddress(File(maybeValidBoardAddress.fileAsInt), maybeValidBoardAddress.rank))
	}
}
