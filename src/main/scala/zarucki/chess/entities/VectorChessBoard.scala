package zarucki.chess.entities

import zarucki.chess.entities.VectorChessBoard.BoardSquares

import scala.collection.mutable

object VectorChessBoard {
	def apply(maxFile: File, maxRank: Int): VectorChessBoard = {
		assert(maxRank >= 1)

		VectorChessBoard(
			boardSquares = Vector.fill(maxRank + 1)(Vector.fill(maxFile.asInt + 1)(FreePeaceful)),
			occupiedSquares = Map(),
			peacefulSquares = (for (rank <- maxRank to 0 by -1; file <- maxFile.asInt to 0 by -1 )
				yield BoardAddress(File(file), rank)
			).toSet
		)
	}

	type BoardSquares = Vector[Vector[BoardSquare]]
}

// TODO: maybe array based chess board could be faster?
case class VectorChessBoard private (
	boardSquares: BoardSquares,
	occupiedSquares: Map[BoardAddress, Piece],
	peacefulSquares: Set[BoardAddress]
) extends ChessBoard {
	assert(boardSquares.size >= 1)

	override lazy val boardMaxFile: File = File(boardSquares(0).size - 1)
	override lazy val boardMaxRank: Int = boardSquares.size - 1

	override type Board = VectorChessBoard

	override def peacefulPlaces(newPiece: Piece): Seq[BoardAddress] = {
		// TODO: maybe dont do this here, when placing we practically do this again
		peacefulSquares.filter { potentialAddress =>
			val potentialDangerZone: Set[BoardAddress] = newPiece.validMovesFor(startAddress = potentialAddress, maxFile = boardMaxFile, maxRank = boardMaxRank).toSet
			val isItSafePositionFoAllRest = occupiedSquares.keys.forall { alreadyPlacedBoardAddress => !potentialDangerZone.contains(alreadyPlacedBoardAddress) }
			//			val isItSafePositionFoAllRest = potentialDangerZone.map(getBoardSquare).forall(!_.isInstanceOf[Occupied])
			isItSafePositionFoAllRest
		}.toSeq
	}

	override def placePiece(address: BoardAddress, pieceToPlace: Piece): VectorChessBoard = {
		tryPlacingMultipleOfSamePiece(Set(address), pieceToPlace) match {
			case Left(msg) => throw new java.lang.AssertionError("assertion failed: " + msg)
			case Right(board) => board
		}
	}

	// This is most inner loop in solution
	override def tryPlacingMultipleOfSamePiece(addresses: Set[BoardAddress], pieceToPlace: Piece): Either[String, VectorChessBoard] = {
		var piecesToPlaceFromSet = mutable.Set(addresses.toSeq :_*)

		// immutables are faster
		var updatedBoardSquares = boardSquares
		var updatedPeacefulSquares = peacefulSquares
		var updatedOccupiedSquares = occupiedSquares

		def markSquareAsNotEmpty(addr: BoardAddress, newBoardSquare: BoardSquare): Unit = {
			updatedBoardSquares = updatedBoardSquares.updated(
				addr.rank,
				updatedBoardSquares(addr.rank).updated(addr.file.asInt, newBoardSquare)
			)
			updatedPeacefulSquares -= addr
		}

		// TODO: make it more pretty than this ugly while?
		while (piecesToPlaceFromSet.nonEmpty) {
			val address = piecesToPlaceFromSet.head

			if (getBoardSquare(address, updatedBoardSquares) != FreePeaceful) {
				return Left(s"Couldn't place piece $pieceToPlace on $address because field was either under attack or occupied.")
			}

			markSquareAsNotEmpty(address, Occupied(pieceToPlace))
			updatedOccupiedSquares = updatedOccupiedSquares.updated(address, pieceToPlace)

			val dangerZone = pieceToPlace.validMovesFor(startAddress = address, maxFile = boardMaxFile, maxRank = boardMaxRank)
			dangerZone.foreach { endangeredAddress =>
				getBoardSquare(endangeredAddress, updatedBoardSquares) match {
					case FreePeaceful => markSquareAsNotEmpty(endangeredAddress, FreeUnderThreat)
					case Occupied(threatenedPiece) => return Left(s"Couldn't place piece $pieceToPlace on $address cause it threatens piece $threatenedPiece on $endangeredAddress.")
					case FreeUnderThreat =>
				}
			}

			piecesToPlaceFromSet = piecesToPlaceFromSet.tail
		}

		Right(
			VectorChessBoard(
				boardSquares = updatedBoardSquares,
				occupiedSquares = updatedOccupiedSquares,
				peacefulSquares = updatedPeacefulSquares
			)
		)
	}

	override def getBoardSquare(address: BoardAddress): BoardSquare = {
		getBoardSquare(address, boardSquares)
	}

	override def getWholeBoardRank(rank: Int): Seq[BoardSquare] = {
		assert(rank <= boardMaxRank && rank >= 0, "Overflow rank.")
		boardSquares(rank)
	}

	protected def getBoardSquare(address: BoardAddress, fromBoardSquares: BoardSquares): BoardSquare = {
		assert(address.file.asInt <= boardMaxFile.asInt, "Overflow file.")
		assert(address.rank <= boardMaxRank, "Overflow rank.")
		fromBoardSquares(address.rank)(address.file.asInt)
	}
}
