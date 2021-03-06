package zarucki.chess.entities

import zarucki.chess.entities.VectorChessBoard.BoardSquares
import File.file2Integer

object VectorChessBoard {
	def apply(maxFile: File, maxRank: Int): VectorChessBoard = {
		assert(maxRank >= 0)

		VectorChessBoard(
			boardSquares = Vector.fill(maxRank + 1)(Vector.fill(maxFile + 1)(FreePeaceful)),
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
	override type Board = VectorChessBoard

	assert(boardSquares.size >= 1)

	override lazy val boardMaxFile: File = File(boardSquares(0).size - 1)
	override lazy val boardMaxRank: Int = boardSquares.size - 1

	private lazy val peacefulSquaresAsSeq = peacefulSquares.toSeq

	override def peacefulPlaces: Seq[BoardAddress] = peacefulSquaresAsSeq

	override def peacefulPlacesForPiece(newPiece: Piece): Seq[BoardAddress] = {
		if (occupiedSquares.isEmpty) {
			peacefulSquaresAsSeq
		} else {
			peacefulSquares.filterNot { potentialAddress =>
				occupiedSquares.keys.exists(occupiedAddress => newPiece.canMoveFromTo(potentialAddress, occupiedAddress))
			}.to
		}
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
					boardToUpdate.boardSquares(addressOfUpdatedSquare.rank).updated(addressOfUpdatedSquare.file, newBoardSquare)
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

				val placedPieceDangerZoneAddresses = pieceToPlace.possibleMoveDestinationsFromPlace(
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

	override def rotate90degrees(): VectorChessBoard = {
		assert(boardMaxFile.asInt == boardMaxRank, "Board is not square.")
		transformWithBoardCenterCoordinates((x,y) => (y,-x))
	}

	override def rotate180degrees(): VectorChessBoard = {
		transformWithBoardCenterCoordinates((x,y) => (-x,-y))
	}

	override def rotate270degrees(): VectorChessBoard = {
		assert(boardMaxFile.asInt == boardMaxRank, "Board is not square.")
		transformWithBoardCenterCoordinates((x,y) => (-y,x))
	}

	// TODO: lazy version, more performant would just rotate inner matrix
	def transformWithBoardCenterCoordinates(transformation: (Double, Double) => (Double, Double)): VectorChessBoard = {
		// point around which we rotate
		val (centerX, centerY) = (boardMaxFile / 2d, boardMaxRank / 2d)

		def translateToNewCoordinateCenter(address: BoardAddress): (Double, Double) = {
			(address.file - centerX, address.rank - centerY)
		}

		def translateBackToOriginalCoordinates(x: Double, y: Double): BoardAddress = {
			BoardAddress(file = File((x + centerX).toInt), rank = (y + centerY).toInt)
		}

		occupiedSquares.foldLeft(VectorChessBoard(maxRank = boardMaxFile, maxFile = File(boardMaxRank))) { case (rotatedBoard, (oldPieceAddress, piece)) =>
			val (translatedX, translatedY) = translateToNewCoordinateCenter(oldPieceAddress)
			val (transformedX, transformedY) = transformation(translatedX, translatedY)
			val newPieceAddress = translateBackToOriginalCoordinates(transformedX, transformedY)
			rotatedBoard.placePiece(piece, newPieceAddress)
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
		assert(address.file <= boardMaxFile, "Overflow file.")
		assert(address.rank <= boardMaxRank, "Overflow rank.")
		fromBoardSquares(address.rank)(address.file)
	}
}
