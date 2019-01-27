package zarucki.chess.entities

import zarucki.chess.UnitSpec
import zarucki.chess.utils.ChessBoardExtensions._

class BoardSpec extends UnitSpec {
	behavior of "Board"

	it should "print correctly" in {
		val board1 = VectorChessBoard(maxFile = File("C"), maxRank = 3)
			.placePiece(King, BoardAddress(File("A"), 0))
			.placePiece(King, BoardAddress(File("A"), 2))
			.placePiece(Rook, BoardAddress(File("C"), 1))
		println(board1.toConsoleString())

		println(VectorChessBoard(maxFile = File("H"), maxRank = 10).toConsoleString())
		println(VectorChessBoard(maxFile = File("H"), maxRank = 8).toConsoleString())

		val board2 = VectorChessBoard(maxFile = File(35), maxRank = 5)
			.placePiece(King, BoardAddress(File("A"), 0))
			.placePiece(King, BoardAddress(File("BB"), 4))
		println(board2.toConsoleString())
		println(board2.toConsoleString(drawThreats = false))
	}

	it should "create a independent copy when placing move" in {
		val emptyBoard = VectorChessBoard(maxFile = File("G"), maxRank = 6)
		val emptyBoardCopy = emptyBoard.copy()
		assert(emptyBoard == emptyBoardCopy)

		val nonEmptyBoard = emptyBoard.placePiece(King, BoardAddress(File("B"), 1))

		assert(emptyBoard != nonEmptyBoard)
		assert(emptyBoardCopy != nonEmptyBoard)
		assert(emptyBoard == emptyBoardCopy)
	}

	it should "be equal to boards with the same pieces" in {
		val emptyBoard = VectorChessBoard(maxFile = File("G"), maxRank = 6)

		assert(
			emptyBoard.placePiece(King, BoardAddress(File("B"), 1))
				==
			emptyBoard.copy().placePiece(King, BoardAddress(File("B"), 1))
		)

		assert(
			VectorChessBoard(maxFile = File("G"), maxRank = 6)
				.placePiece(King, BoardAddress(File("A"), 5))
				.placePiece(King, BoardAddress(File("B"), 2))
				.placePiece(Queen, BoardAddress(File("C"), 0))
				==
			VectorChessBoard(maxFile = File("G"), maxRank = 6)
				.placePiece(Queen, BoardAddress(File("C"), 0))
				.placePiece(King, BoardAddress(File("B"), 2))
				.placePiece(King, BoardAddress(File("A"), 5))
		)
	}

	it should "throw assert error when placing piece on square that is already occupied" in {
		val emptyBoard = VectorChessBoard(maxFile = File("C"), maxRank = 2)
		val boardWithOneQueen = emptyBoard.placePiece(Queen, BoardAddress(File("A"), 0))
		assertThrows[AssertionError](
			boardWithOneQueen.placePiece(King, BoardAddress(File("A"), 0))
		)
	}

	it should "throw assert error when placing piece on square that is under threat" in {
		val emptyBoard = VectorChessBoard(maxFile = File("C"), maxRank = 2)
		val boardWithOneQueen = emptyBoard.placePiece(Queen, BoardAddress(File("A"), 0))
		assertThrows[AssertionError](
			boardWithOneQueen.placePiece(King, BoardAddress(File("B"), 0))
		)
		assertThrows[AssertionError](
			boardWithOneQueen.placePiece(King, BoardAddress(File("A"), 1))
		)
		assertThrows[AssertionError](
			boardWithOneQueen.placePiece(King, BoardAddress(File("B"), 1))
		)
	}
}
