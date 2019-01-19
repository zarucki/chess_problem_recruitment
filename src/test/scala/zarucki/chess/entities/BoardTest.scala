package zarucki.chess.entities

import zarucki.chess.UnitSpec

// TODO: test copying
// TODO: test comparision
class BoardTest extends UnitSpec {

	behavior of "BoardTest"

	it should "toString" in {
		val board1 = new Board(files = 3, ranks = 3)
		board1.placePiece(King(), BoardAddress(File("A"), 0))
		board1.placePiece(King(), BoardAddress(File("A"), 2))
		board1.placePiece(Rook(), BoardAddress(File("C"), 1))
		println(board1.toString)

		val board2 = new Board(files = 8, ranks = 11)
		println(board2.toString)

		val board3 = new Board(files = 8, ranks = 8)
		println(board3.toString)

		val board4 = new Board(files = 35, ranks = 5)
		board4.placePiece(King(), BoardAddress(File("A"), 0))
		board4.placePiece(King(), BoardAddress(File("BB"), 4))
		println(board4.toString)
	}
}
