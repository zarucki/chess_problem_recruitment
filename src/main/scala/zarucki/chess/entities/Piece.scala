package zarucki.chess.entities

import File.file2Integer
import zarucki.chess.entities.Piece.{MaybeValidBoardAddress, OneStepMovement}

object Piece {
	object MaybeValidBoardAddress {
		def apply(boardAddress: BoardAddress): MaybeValidBoardAddress = {
			MaybeValidBoardAddress(fileAsInt = boardAddress.file, rank = boardAddress.rank)
		}
	}

	case class MaybeValidBoardAddress(fileAsInt: Int, rank: Int)

	type OneStepMovement = MaybeValidBoardAddress => MaybeValidBoardAddress

	val toNorth: OneStepMovement = a => a.copy(rank = a.rank + 1)
	val toEast: OneStepMovement = a => a.copy(fileAsInt = a.fileAsInt + 1)
	val toSouth: OneStepMovement = a => a.copy(rank = a.rank - 1)
	val toWest: OneStepMovement = a => a.copy(fileAsInt = a.fileAsInt - 1)

	val toNorthEast: OneStepMovement = toNorth.andThen(toEast)
	val toNorthWest: OneStepMovement = toNorth.andThen(toWest)
	val toSouthEast: OneStepMovement = toSouth.andThen(toEast)
	val toSouthWest: OneStepMovement = toSouth.andThen(toWest)
}

trait Piece {
	def representation: Char

	def canMoveFromTo(fromAddress: BoardAddress, toAddress: BoardAddress): Boolean
	protected def possibleMovesGenerators(address: BoardAddress): Seq[Stream[MaybeValidBoardAddress]]

	// TODO: should this still be a stream? it is not infinite in nature!
	def possibleMoveDestinationsFromPlace(startAddress: BoardAddress, maxFile: File, maxRank: Int): Stream[BoardAddress] = {
		possibleMovesGenerators(startAddress)
			.map(readValidBoardAddresses(_, maxFile, maxRank))
			.reduce(_ ++ _)
	}

	override final def toString: String = {
		representation.toUpper.toString
	}

	protected def isMoveDestinationWithinBoard(addr: MaybeValidBoardAddress, maxFile: File, maxRank: Int): Boolean = {
		addr.rank <= maxRank && addr.rank >= 0 && addr.fileAsInt <= maxFile && addr.fileAsInt >= 0
	}

	protected def readValidBoardAddresses(moveStream: Stream[MaybeValidBoardAddress], maxFile: File, maxRank: Int): Stream[BoardAddress] = {
		moveStream
			.filter(isMoveDestinationWithinBoard(_, maxFile, maxRank))
			.map(maybeValidBoardAddress => BoardAddress(File(maybeValidBoardAddress.fileAsInt), maybeValidBoardAddress.rank))
	}
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
