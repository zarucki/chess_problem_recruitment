package zarucki.chess.entities

import zarucki.chess.entities.Piece._

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

	override def canMoveFromTo(fromAddress: BoardAddress, toAddress: BoardAddress): Boolean = {
		val addressDelta = fromAddress.getAddressDelta(toAddress)
		addressDelta.rankDelta <= 1 && addressDelta.fileDelta <= 1
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

	override def canMoveFromTo(fromAddress: BoardAddress, toAddress: BoardAddress): Boolean = {
		val addressDelta = fromAddress.getAddressDelta(toAddress)
		(addressDelta.rankDelta == 1 && addressDelta.fileDelta == 2) ||
			(addressDelta.rankDelta == 2 && addressDelta.fileDelta == 1)
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

	override def canMoveFromTo(fromAddress: BoardAddress, toAddress: BoardAddress): Boolean = {
		val addressDelta = fromAddress.getAddressDelta(toAddress)
		addressDelta.rankDelta == 0 || addressDelta.fileDelta == 0 || addressDelta.rankDelta == addressDelta.fileDelta
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

	override def canMoveFromTo(fromAddress: BoardAddress, toAddress: BoardAddress): Boolean = {
		val addressDelta = fromAddress.getAddressDelta(toAddress)
		addressDelta.rankDelta == 0 || addressDelta.fileDelta == 0
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

	override def canMoveFromTo(fromAddress: BoardAddress, toAddress: BoardAddress): Boolean = {
		val addressDelta = fromAddress.getAddressDelta(toAddress)
		addressDelta.rankDelta == addressDelta.fileDelta
	}
}
