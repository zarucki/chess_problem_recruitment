package zarucki.chess.entities

import zarucki.chess.UnitSpec
import zarucki.chess.utils.ChessBoardExtensions._

class BoardSpec extends UnitSpec {
	behavior of "Board"

	it should "print correctly" in {
		val board1 = VectorChessBoard(maxFile = File("C"), maxRank = 3)
			.placePiece(BoardAddress(File("A"), 0), King)
			.placePiece(BoardAddress(File("A"), 2), King)
			.placePiece(BoardAddress(File("C"), 1), Rook)
		println(board1.toConsoleString())

		println(VectorChessBoard(maxFile = File("H"), maxRank = 10).toConsoleString())
		println(VectorChessBoard(maxFile = File("H"), maxRank = 8).toConsoleString())

		val board2 = VectorChessBoard(maxFile = File(35), maxRank = 5)
			.placePiece(BoardAddress(File("A"), 0), King)
			.placePiece(BoardAddress(File("BB"), 4), King)
		println(board2.toConsoleString())
		println(board2.toConsoleString(drawThreats = false))
	}

	it should "create a independent copy when placing move" in {
		val emptyBoard = VectorChessBoard(maxFile = File("G"), maxRank = 6)
		val emptyBoardCopy = emptyBoard.copy()
		assert(emptyBoard == emptyBoardCopy)

		val nonEmptyBoard = emptyBoard.placePiece(BoardAddress(File("B"), 1), King)

		assert(emptyBoard != nonEmptyBoard)
		assert(emptyBoardCopy != nonEmptyBoard)
		assert(emptyBoard == emptyBoardCopy)
	}

	it should "be equal to boards with the same pieces" in {
		val emptyBoard = VectorChessBoard(maxFile = File("G"), maxRank = 6)

		assert(
			emptyBoard.placePiece(BoardAddress(File("B"), 1), King)
				==
			emptyBoard.copy().placePiece(BoardAddress(File("B"), 1), King)
		)

		assert(
			VectorChessBoard(maxFile = File("G"), maxRank = 6)
				.placePiece(BoardAddress(File("A"), 5), King)
				.placePiece(BoardAddress(File("B"), 2), King)
				.placePiece(BoardAddress(File("C"), 0), Queen)
				==
			VectorChessBoard(maxFile = File("G"), maxRank = 6)
				.placePiece(BoardAddress(File("C"), 0), Queen)
				.placePiece(BoardAddress(File("B"), 2), King)
				.placePiece(BoardAddress(File("A"), 5), King)
		)
	}

	it should "throw assert error when placing piece on square that is already occupied" in {
		val emptyBoard = VectorChessBoard(maxFile = File("C"), maxRank = 2)
		val boardWithOneQueen = emptyBoard.placePiece(BoardAddress(File("A"), 0), Queen)
		assertThrows[AssertionError](
			boardWithOneQueen.placePiece(BoardAddress(File("A"), 0), King)
		)
	}

	it should "throw assert error when placing piece on square that is under threat" in {
		val emptyBoard = VectorChessBoard(maxFile = File("C"), maxRank = 2)
		val boardWithOneQueen = emptyBoard.placePiece(BoardAddress(File("A"), 0), Queen)
		assertThrows[AssertionError](
			boardWithOneQueen.placePiece(BoardAddress(File("B"), 0), King)
		)
		assertThrows[AssertionError](
			boardWithOneQueen.placePiece(BoardAddress(File("A"), 1), King)
		)
		assertThrows[AssertionError](
			boardWithOneQueen.placePiece(BoardAddress(File("B"), 1), King)
		)
	}
}
