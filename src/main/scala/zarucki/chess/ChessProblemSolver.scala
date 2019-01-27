package zarucki.chess

import zarucki.chess.entities._

import scala.annotation.tailrec

// TODO: use some logger instead of println
// TODO: measure statistics like time and amount of possibilities checked
// TODO: pallarel akka?
object ChessProblemSolver {

	private def pieceToWeight(piece: Piece): Int = {
		piece match {
			case _: Queen => 5
			case _: Rook => 4
			case _: Bishop => 3
			case _: Knight => 2
			case _: King => 1
		}
	}

	def solveNonThreatenProblem(boardFiles: Int, boardRanks: Int, chessPiecesToPlace: Seq[Piece]): List[ChessBoard] = {
		val emptyBoard = VectorChessBoard(maxFile = File(boardFiles - 1), maxRank = boardRanks - 1)

		val groupedAndSortedChessPieces: Seq[Seq[Piece]] = chessPiecesToPlace
			.groupBy(_.representation)
			.map(_._2)
			.toList
			.sortBy(o => -1 * pieceToWeight(o.head))

//		solveProblemWithoutTailRecursion(emptyBoard, groupedAndSortedChessPieces)
		solveProblemWithTailRecursion(emptyBoard :: Nil, groupedAndSortedChessPieces)
	}

	private def solveProblemWithoutTailRecursion(currentBoardState: ChessBoard, leftChessPiecesToPlace: Seq[Seq[Piece]]): List[ChessBoard] = {
		if (leftChessPiecesToPlace.isEmpty) {
			List(currentBoardState)
		} else {
			val piecesToPlaceOfGivenType = leftChessPiecesToPlace.head.toList
			val nonThreatenedFields = currentBoardState.peacefulPlaces(piecesToPlaceOfGivenType.head).toSet
			if (nonThreatenedFields.isEmpty || nonThreatenedFields.size < piecesToPlaceOfGivenType.size) {
				// this is dead end, we need to take step back
				List.empty
			} else {
				val pieceToPlace = piecesToPlaceOfGivenType.head

				// we are placing pieces of one type at once to not consider the same combinations
				combinationsOfN(nonThreatenedFields, piecesToPlaceOfGivenType.size).flatMap {
					case oneElementSet if oneElementSet.size == 1 =>
						solveProblemWithoutTailRecursion(currentBoardState.placePiece(oneElementSet.head, pieceToPlace), leftChessPiecesToPlace.tail)
					case multiElementSet =>
						currentBoardState.tryPlacingMultipleOfSamePiece(multiElementSet, pieceToPlace) match {
							case Right(newBoard) => solveProblemWithoutTailRecursion(newBoard, leftChessPiecesToPlace.tail)
							case _ => List.empty
						}
				}.toList
			}
		}
	}

	 @tailrec
	private def solveProblemWithTailRecursion(chessBoards: List[ChessBoard], leftChessPiecesToPlace: Seq[Seq[Piece]]): List[ChessBoard] = {
		if (leftChessPiecesToPlace.isEmpty) {
			chessBoards
		} else {
			val piecesToPlaceOfGivenType = leftChessPiecesToPlace.head.toList

			val newChessBoards = chessBoards.flatMap { currentBoard =>
				val nonThreatenedFields = currentBoard.peacefulPlaces(piecesToPlaceOfGivenType.head).toSet

				if (nonThreatenedFields.isEmpty || nonThreatenedFields.size < piecesToPlaceOfGivenType.size) {
					// this is dead end, we need to take step back
					List.empty
				} else {
					val pieceToPlace = piecesToPlaceOfGivenType.head

					// we are placing pieces of one type at once to not consider the same combinations
					combinationsOfN(nonThreatenedFields, piecesToPlaceOfGivenType.size).flatMap {
						case oneElementSet if oneElementSet.size == 1 =>
							Some(currentBoard.placePiece(oneElementSet.head, pieceToPlace))
						case multiElementSet =>
							currentBoard.tryPlacingMultipleOfSamePiece(multiElementSet, pieceToPlace) match {
								case Right(newBoard) => Some(newBoard)
								case _ => None
							}
					}.toList
				}
			}
			solveProblemWithTailRecursion(newChessBoards, leftChessPiecesToPlace.tail)
		}
	}

	private def combinationsOfN(boardMoves: Set[BoardAddress], n: Int): Iterator[Set[BoardAddress]] = {
		if (n > 1) {
			boardMoves.subsets(n)
		} else {
			boardMoves.map(Set(_)).toIterator
		}
	}
}
