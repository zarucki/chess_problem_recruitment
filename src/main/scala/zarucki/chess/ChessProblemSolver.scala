package zarucki.chess

import zarucki.chess.entities._

import scala.annotation.tailrec

// TODO: use some logger instead of println
// TODO: measure statistics like time and amount of possibilities checked
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

	def solveNonThreatenProblem(boardFiles: Int, boardRanks: Int, chessPiecesToPlace: Seq[Piece], parallel: Boolean = true): List[ChessBoard] = {
		assert(chessPiecesToPlace.nonEmpty)
		val emptyBoard = VectorChessBoard(maxFile = File(boardFiles - 1), maxRank = boardRanks - 1)

		val groupedAndSortedChessPieces = chessPiecesToPlace
			.groupBy(_.representation)
			.map { case (_, pieces) => (pieces.head, pieces.size)}
			.toList
			.sortBy { case (piece, _) => -1 * pieceToWeight(piece) }

		solveProblemWithTailRecursion(List(emptyBoard), groupedAndSortedChessPieces, parallel)
	}

	 @tailrec
	private def solveProblemWithTailRecursion(chessBoards: List[ChessBoard], leftChessPiecesToPlace: Seq[(Piece, Int)], parallel :Boolean): List[ChessBoard] = {
		if (leftChessPiecesToPlace.isEmpty) {
			chessBoards
		} else {
			val (piece, pieceCount) = leftChessPiecesToPlace.head

			val chessBoardsToConsider = if (parallel) chessBoards.par else chessBoards
			val newChessBoards: List[ChessBoard] = chessBoardsToConsider.flatMap { currentBoard =>
				val nonThreatenedFields = currentBoard.peacefulPlaces(piece).toSet

				if (nonThreatenedFields.isEmpty || nonThreatenedFields.size < pieceCount) {
					List.empty
				} else {
					// we are placing pieces of one type at once to not consider the same combinations in different order
					combinationsOfN(nonThreatenedFields, pieceCount).flatMap {
						case oneElementSet if oneElementSet.size == 1 =>
							Some(currentBoard.placePiece(oneElementSet.head, piece))
						case multiElementSet =>
							currentBoard.tryPlacingMultipleOfSamePiece(multiElementSet, piece) match {
								case Right(newBoard) => Some(newBoard)
								case _ => None
							}
					}.toList
				}
			}.toList

			solveProblemWithTailRecursion(newChessBoards, leftChessPiecesToPlace.tail, parallel)
		}
	}

	private def combinationsOfN(boardMoves: Set[BoardAddress], n: Int): Iterator[Set[BoardAddress]] = {
		if (n > 1) {
			// TODO: maybe own implementation would be faster?
			boardMoves.subsets(n)
		} else {
			boardMoves.map(Set(_)).toIterator
		}
	}
}
