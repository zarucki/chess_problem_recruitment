package zarucki.chess

import zarucki.chess.entities._

import scala.annotation.tailrec
import scala.collection.parallel.ParSeq

object ChessProblemSolver {
	// TODO: count this threshold based on the board size?
	// Combination number grows fast n - number of squares, k pieces placed at once n! / (k! * (n - k)!)
	// making optimization longer than placing those pieces one by one
	private val maxNumberOfPiecesPlacedAtOnce = 4

	private lazy val pieceToWeight: Map[Piece, Int] = Map(
		Queen -> 5,
		Rook -> 4,
		Bishop -> 3,
		Knight -> 2,
		King -> 1
	)

	def solveNonThreatenProblem(boardFiles: Int, boardRanks: Int, chessPiecesToPlace: Seq[Piece]): List[ChessBoard] = {
		assert(chessPiecesToPlace.nonEmpty, "Empty piece list.")

		val emptyBoard = VectorChessBoard(maxFile = File(boardFiles - 1), maxRank = boardRanks - 1)

		val groupedAndSortedChessPieces = chessPiecesToPlace
			.groupBy(_.representation)
			.toList
			.flatMap {
				case (_, pieces) if pieces.size <= maxNumberOfPiecesPlacedAtOnce => Seq((pieces.head, pieces.size))
				case (_, pieces) =>
					val totalPiecesToPlace = pieces.size
					val rest = totalPiecesToPlace % maxNumberOfPiecesPlacedAtOnce
					val pieceType = pieces.head

					Seq.fill(totalPiecesToPlace / maxNumberOfPiecesPlacedAtOnce)((pieceType, maxNumberOfPiecesPlacedAtOnce)) ++
						(if (rest > 0) Seq((pieceType, rest)) else Seq.empty)
			}
			.sortBy { case (piece, _) => -1 * pieceToWeight(piece) }


		val resultChessBoards = solveProblemWithTailRecursion(ParSeq(emptyBoard), groupedAndSortedChessPieces)

		val thereWasGroupSplit = groupedAndSortedChessPieces
			.groupBy { case (pieceType, _) => pieceType }
			.exists { case (_, group) => group.size > 1}

		if (thereWasGroupSplit) {
			// Because we placed piece of one type in multiple steps we will have some duplicate results
			resultChessBoards.toSet.to
		} else {
			resultChessBoards
		}
	}

	 @tailrec
	private def solveProblemWithTailRecursion(chessBoards: ParSeq[ChessBoard], chessPiecesToPlace: Seq[(Piece, Int)]): List[ChessBoard] = {
		 chessPiecesToPlace match {
			 case Nil => chessBoards.to
			 case (pieceType, pieceCount) :: restOfPiecesToPlace =>
				 val allLeftPiecesToPlace: Int = chessPiecesToPlace.foldLeft(0)(_ + _._2)

				 val chessBoardsAfterAddingPiecesOfType = chessBoards
					 .map(currentBoard => (currentBoard, currentBoard.peacefulPlacesForPiece(pieceType).toSet))
					 .filter { case (currentBoard, peacefulPlacesForPiece) =>
						 peacefulPlacesForPiece.size >= pieceCount && currentBoard.peacefulPlaces.size >= allLeftPiecesToPlace
					 }
					 .flatMap { case (currentBoard, peacefulPlacesForPiece) =>
						 allNonRepeatingCombinationsOfN(peacefulPlacesForPiece, pieceCount).flatMap {
							 case singleAddressSet if singleAddressSet.size == 1 =>
								 Some(currentBoard.placePiece(pieceType, singleAddressSet.head))
							 case multiAddressSet =>
								 currentBoard.tryPlacingMultipleOfSamePiece(pieceType, multiAddressSet.to :_*).toOption
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
			boardMoves.map(Set(_)).to
		}
	}
}
