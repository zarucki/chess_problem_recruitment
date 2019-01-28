package zarucki.chess

import zarucki.chess.entities._

import scala.annotation.tailrec
import scala.collection.parallel.ParSeq

// TODO: stats how many iterations, time
// TODO: measure statistics like time and amount of possibilities checked
object ChessProblemSolver {
	private lazy val pieceToWeight: Map[Piece, Int] = Map(
		Queen -> 5,
		Rook -> 4,
		Bishop -> 3,
		Knight -> 2,
		King -> 1
	)

	def solveNonThreatenProblem(boardFiles: Int, boardRanks: Int, chessPiecesToPlace: Seq[Piece]): List[ChessBoard] = {
		assert(chessPiecesToPlace.nonEmpty)
		val emptyBoard = VectorChessBoard(maxFile = File(boardFiles - 1), maxRank = boardRanks - 1)

		val groupedAndSortedChessPieces = chessPiecesToPlace
			.groupBy(_.representation)
			.map { case (_, pieces) => (pieces.head, pieces.size)}
			.toList
			.sortBy { case (piece, _) => -1 * pieceToWeight(piece) }

		solveProblemWithTailRecursion(ParSeq(emptyBoard), groupedAndSortedChessPieces)
	}

	 @tailrec
	private def solveProblemWithTailRecursion(chessBoards: ParSeq[ChessBoard], chessPiecesToPlace: Seq[(Piece, Int)]): List[ChessBoard] = {
		 chessPiecesToPlace match {
			 case Nil => chessBoards.toList
			 case (pieceType, pieceCount) :: restOfPiecesToPlace =>

				 val chessBoardsAfterAddingPiecesOfType: ParSeq[ChessBoard] = chessBoards
					 .map(currentBoard => (currentBoard, currentBoard.peacefulPlacesForPiece(pieceType).toSet))
					 .filter { case (_, peacefulPlacesForPiece) => peacefulPlacesForPiece.size >= pieceCount }
					 .flatMap { case (currentBoard, peacefulPlacesForPiece) =>
						 allNonRepeatingCombinationsOfN(peacefulPlacesForPiece, pieceCount).flatMap {
							 case singleAddressSet if singleAddressSet.size == 1 =>
								 Some(currentBoard.placePiece(pieceType, singleAddressSet.head))
							 case multiAddressSet =>
								 currentBoard.tryPlacingMultipleOfSamePiece(pieceType, multiAddressSet.toSeq :_*).toOption
						 }
					 }

				 solveProblemWithTailRecursion(chessBoardsAfterAddingPiecesOfType, restOfPiecesToPlace)
		 }
	}

	private def allNonRepeatingCombinationsOfN(boardMoves: Set[BoardAddress], n: Int): Iterator[Set[BoardAddress]] = {
		if (n > 1) {
			// TODO: maybe own implementation would be faster?
			boardMoves.subsets(n)
		} else {
			boardMoves.map(Set(_)).toIterator
		}
	}
}
