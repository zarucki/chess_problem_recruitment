package zarucki.chess

import zarucki.chess.entities._
import zarucki.chess.utils.ChessBoardExtensions.consolePrintableChessBoard

// TODO: print something when no valid solutions
// TODO: stats how many iterations, time
class ChessSolverSpec extends UnitSpec {

	it should "correctly solve 3×3 board containing 2 Kings and 1 Rook" in {
		val result: Set[ChessBoard] = ChessProblemSolver.solveNonThreatenProblem(
			boardFiles = 3,
			boardRanks = 3,
			chessPiecesToPlace = pieceConfiguration(kingCount = 2, rookCount = 1)
		).toSet

		assert(result == Set(
			VectorChessBoard(maxFile = File("C"), maxRank = 2)
				.placePiece(BoardAddress(File("A"), 0), King())
				.placePiece(BoardAddress(File("A"), 2), King())
				.placePiece(BoardAddress(File("C"), 1), Rook()),
			VectorChessBoard(maxFile = File("C"), maxRank = 2)
				.placePiece(BoardAddress(File("C"), 0), King())
				.placePiece(BoardAddress(File("C"), 2), King())
				.placePiece(BoardAddress(File("A"), 1), Rook()),
			VectorChessBoard(maxFile = File("C"), maxRank = 2)
				.placePiece(BoardAddress(File("A"), 2), King())
				.placePiece(BoardAddress(File("C"), 2), King())
				.placePiece(BoardAddress(File("B"), 0), Rook()),
			VectorChessBoard(maxFile = File("C"), maxRank = 2)
				.placePiece(BoardAddress(File("A"), 0), King())
				.placePiece(BoardAddress(File("C"), 0), King())
				.placePiece(BoardAddress(File("B"), 2), Rook())
		))
	}

	it should "correctly solve 4×4 board containing 2 Rooks and 4 Knights" in {
		val result: Set[ChessBoard] = ChessProblemSolver.solveNonThreatenProblem(
			boardFiles = 4,
			boardRanks = 4,
			chessPiecesToPlace = pieceConfiguration(rookCount = 2, knightCount = 4)
		).toSet

		assert(result.contains(
			VectorChessBoard(maxFile = File("D"), maxRank = 3)
				.placePiece(BoardAddress(File("A"), 0), Rook())
				.placePiece(BoardAddress(File("C"), 2), Rook())
				.placePiece(BoardAddress(File("B"), 1), Knight())
				.placePiece(BoardAddress(File("B"), 3), Knight())
				.placePiece(BoardAddress(File("D"), 1), Knight())
				.placePiece(BoardAddress(File("D"), 3), Knight())
		))
		assert(result.size == 8)
	}

	it should "correctly solve 5×5 board containing 2 kings, 2 rooks and 1 Knight" in {
		val result: List[ChessBoard] = ChessProblemSolver.solveNonThreatenProblem(
			boardFiles = 5,
			boardRanks = 5,
			chessPiecesToPlace = pieceConfiguration(kingCount = 2, rookCount = 2, knightCount = 1)
		)

		println(result.head.toConsoleString)
		println(result.size)
		assert(result.size == 2374)
	}

	it should "correctly solve 5×5 board containing 2 queens, 2 bishops, 1 rooks and 1 Knight" in {
		val result: List[ChessBoard] = ChessProblemSolver.solveNonThreatenProblem(
			boardFiles = 5,
			boardRanks = 5,
			chessPiecesToPlace = pieceConfiguration(queenCount = 2, bishopCount = 2, rookCount = 1, knightCount = 1)
		)

		println(result.head.toConsoleString)
		println(result.size)
		assert(result.size == 104)
	}

	it should "correctly solve 4×4 board containing 2 queens" in {
		val result: List[ChessBoard] = ChessProblemSolver.solveNonThreatenProblem(
			boardFiles = 4,
			boardRanks = 4,
			chessPiecesToPlace = pieceConfiguration(queenCount = 2)
		)

		println(result.head.toConsoleString)
		println(result.size)
		assert(result.size == 44)
	}

	it should "correctly solve 7×7 board containing 2 Kings, 2 Queens, 2 Bishops" in {
		val result: List[ChessBoard] = ChessProblemSolver.solveNonThreatenProblem(
			boardFiles = 7,
			boardRanks = 7,
			chessPiecesToPlace = pieceConfiguration(kingCount = 2, queenCount = 2, bishopCount = 2)
		)

		println(result.head.toConsoleString)
		println(result.size)
		assert(result.size == 1380952)
	}

	it should "correctly solve 7×7 board containing 2 Kings, 2 Queens, 2 Bishops, 1 Knight" in {
		val result: List[ChessBoard] = ChessProblemSolver.solveNonThreatenProblem(
			boardFiles = 7,
			boardRanks = 7,
			chessPiecesToPlace = pieceConfiguration(kingCount = 2, queenCount = 2, bishopCount = 2, knightCount = 1)
		)

		println(result.head.toConsoleString)
		println(result.size)
		assert(result.size == 3063828)
	}

	it should "correctly solve 7×7 board containing 2 Kings, 2 Queens, 2 Bishops, 1 Knight without parallel" in {
		val result: List[ChessBoard] = ChessProblemSolver.solveNonThreatenProblem(
			boardFiles = 7,
			boardRanks = 7,
			chessPiecesToPlace = pieceConfiguration(kingCount = 2, queenCount = 2, bishopCount = 2, knightCount = 1),
			parallel = false
		)

		println(result.head.toConsoleString)
		println(result.size)
		assert(result.size == 3063828)
	}

	def pieceConfiguration(kingCount: Int = 0, queenCount: Int = 0, bishopCount: Int = 0, rookCount: Int = 0, knightCount: Int = 0): Seq[Piece] = {
		(0 until kingCount).map(_ => King()) ++
		(0 until queenCount).map(_ => Queen()) ++
		(0 until bishopCount).map(_ => Bishop()) ++
		(0 until rookCount).map(_ => Rook()) ++
		(0 until knightCount).map(_ => Knight())
	}
}
