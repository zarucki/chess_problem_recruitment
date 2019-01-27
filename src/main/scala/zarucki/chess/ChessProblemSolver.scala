package zarucki.chess

import zarucki.chess.entities._

import scala.annotation.tailrec
import scala.collection.parallel.ParSeq

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

		solveProblemWithTailRecursion(ParSeq(emptyBoard), groupedAndSortedChessPieces)
	}

	 @tailrec
	private def solveProblemWithTailRecursion(chessBoards: ParSeq[ChessBoard], leftChessPiecesToPlace: Seq[Seq[Piece]]): List[ChessBoard] = {
		if (leftChessPiecesToPlace.isEmpty) {
			chessBoards.toList
		} else {
			val piecesToPlaceOfGivenType = leftChessPiecesToPlace.head.toList

			val newChessBoards: ParSeq[ChessBoard] = chessBoards.flatMap { currentBoard =>
				val nonThreatenedFields = currentBoard.peacefulPlaces(piecesToPlaceOfGivenType.head).toSet

				if (nonThreatenedFields.isEmpty || nonThreatenedFields.size < piecesToPlaceOfGivenType.size) {
					List.empty
				} else {
					val pieceToPlace = piecesToPlaceOfGivenType.head

					// we are placing pieces of one type at once to not consider the same combinations in different order
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
			// TODO: maybe own implementation would be faster?
			boardMoves.subsets(n)
		} else {
			boardMoves.map(Set(_)).toIterator
		}
	}
}
