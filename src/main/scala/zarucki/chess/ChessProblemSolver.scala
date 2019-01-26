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

		actuallySolveTheProblem(emptyBoard, groupedAndSortedChessPieces)
	}

	// TODO: make it tailrec
	// @tailrec
	private def actuallySolveTheProblem(currentBoardState: ChessBoard, leftChessPiecesToPlace: Seq[Seq[Piece]]): List[ChessBoard] = {
		if (leftChessPiecesToPlace.isEmpty) {
			List(currentBoardState)
			// TODO: add other symetric configurations as results
		} else {
			// TODO: precheck if/filter something was already considered? because is symmetric
			val piecesToPlaceOfGivenType = leftChessPiecesToPlace.head.toList
			val nonThreatenedFields = currentBoardState.peacefulPlaces(piecesToPlaceOfGivenType.head).toSet
			if (nonThreatenedFields.isEmpty || nonThreatenedFields.size < piecesToPlaceOfGivenType.size) {
				// this is dead end, we need to take step back
				// TODO: mark other symmetric things as dead ends
				List.empty
			} else {
				val pieceToPlace = piecesToPlaceOfGivenType.head

				combinationsOfN(nonThreatenedFields, piecesToPlaceOfGivenType.size).flatMap {
					case oneElementSet if oneElementSet.size == 1 =>
						actuallySolveTheProblem(currentBoardState.placePiece(oneElementSet.head, pieceToPlace), leftChessPiecesToPlace.tail)
					case multiElementSet =>
						currentBoardState.tryPlacingMultipleOfSamePiece(multiElementSet, pieceToPlace) match {
							case Right(newBoard) => actuallySolveTheProblem(newBoard, leftChessPiecesToPlace.tail)
							case _ => List.empty
						}
				}.toList
			}
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
