package zarucki.chess.entities

import zarucki.chess.entities.VectorChessBoard.BoardSquares

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

	override def placePiece(pieceToPlace: Piece, newPieceAddress: BoardAddress): VectorChessBoard = {
		tryPlacingMultipleOfSamePiece(pieceToPlace, newPieceAddress) match {
			case Left(msg) => throw new java.lang.AssertionError("assertion failed: " + msg)
			case Right(board) => board
		}
	}

	override def tryPlacingMultipleOfSamePiece(pieceToPlace: Piece, newPieceAddresses: BoardAddress*): Either[String, VectorChessBoard] = {
		def boardWithUpdatedSquare(boardToUpdate: VectorChessBoard, addressOfUpdatedSquare: BoardAddress, newBoardSquare: BoardSquare, markAddressAsOccupied: Boolean = false) = {
			VectorChessBoard(
				boardSquares = boardToUpdate.boardSquares.updated(
					addressOfUpdatedSquare.rank,
					boardToUpdate.boardSquares(addressOfUpdatedSquare.rank).updated(addressOfUpdatedSquare.file.asInt, newBoardSquare)
				),
				occupiedSquares = if (markAddressAsOccupied) boardToUpdate.occupiedSquares.updated(addressOfUpdatedSquare, pieceToPlace) else boardToUpdate.occupiedSquares,
				peacefulSquares = boardToUpdate.peacefulSquares - addressOfUpdatedSquare,
			)
		}

		newPieceAddresses.foldLeft[Either[String, VectorChessBoard]](Right(this)) {
			case (l @ Left(_), _) => l
			case (Right(currentChessBoard), addressOfNewPiece) if getBoardSquare(addressOfNewPiece, currentChessBoard.boardSquares) != FreePeaceful =>
				Left(s"Couldn't place piece $pieceToPlace on $addressOfNewPiece because field was either under attack or occupied.")
			case (Right(currentChessBoard), addressOfNewPiece) =>
				val boardAfterPlacingPiece = boardWithUpdatedSquare(currentChessBoard, addressOfNewPiece, Occupied(pieceToPlace), markAddressAsOccupied = true)

				val placedPieceDangerZoneAddresses = pieceToPlace.validMovesFor(
					startAddress = addressOfNewPiece,
					maxFile = boardMaxFile,
					maxRank = boardMaxRank
				)

				placedPieceDangerZoneAddresses
					.map(addressUnderThreat => (addressUnderThreat, getBoardSquare(addressUnderThreat, boardAfterPlacingPiece.boardSquares)))
					.foldLeft[Either[String, VectorChessBoard]](Right(boardAfterPlacingPiece)) {
						case (l @ Left(_), _) => l
						case (_, (addressUnderThreat, Occupied(threatenedPiece))) =>
							Left(s"Couldn't place piece $pieceToPlace on $addressOfNewPiece cause it threatens piece $threatenedPiece on $addressUnderThreat.")
						case (Right(chessBoardAfterPlacingPieceAndThreatUpdates), (addressUnderThreat, FreePeaceful)) =>
							Right(boardWithUpdatedSquare(chessBoardAfterPlacingPieceAndThreatUpdates, addressUnderThreat, FreeUnderThreat))
						case (r @ Right(_), (_, FreeUnderThreat)) => r
					}
		}
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
