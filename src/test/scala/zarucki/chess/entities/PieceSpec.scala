package zarucki.chess.entities

import zarucki.chess.UnitSpec

class PieceSpec extends UnitSpec {
	behavior of "Queen"

	it should "properly move in all directions all the squares" in {
		val queenValidMoves = Queen.validMovesFor(BoardAddress(File("C"), 2), maxFile = File("E"), maxRank = 4)
		assert(queenValidMoves.toSet == Set(
			BoardAddress(File("D"), 2), BoardAddress(File("E"), 2),
			BoardAddress(File("B"), 2), BoardAddress(File("A"), 2),
			BoardAddress(File("C"), 3), BoardAddress(File("C"), 4),
			BoardAddress(File("C"), 1), BoardAddress(File("C"), 0),
			BoardAddress(File("D"), 3), BoardAddress(File("E"), 4),
			BoardAddress(File("B"), 1), BoardAddress(File("A"), 0),
			BoardAddress(File("B"), 3), BoardAddress(File("A"), 4),
			BoardAddress(File("D"), 1), BoardAddress(File("E"), 0)
		))
	}

	it should "correctly handle being in the corner" in {
		val queenValidMoves = Queen.validMovesFor(BoardAddress(File("A"), 0), maxFile = File("C"), maxRank = 2)
		assert(queenValidMoves.toSet == Set(
			BoardAddress(File("A"), 1), BoardAddress(File("A"), 2),
			BoardAddress(File("B"), 0), BoardAddress(File("C"), 0),
			BoardAddress(File("B"), 1), BoardAddress(File("C"), 2)
		))
	}

	it should "correctly handle 2 x 1 chess board" in {
		val queenValidMoves = Queen.validMovesFor(BoardAddress(File("B"), 0), maxFile = File("B"), maxRank = 0)
		assert(queenValidMoves.toSet == Set(BoardAddress(File("A"), 0)))
	}

	it should "correctly handle 1 x 1 chess board" in {
		val queenValidMoves = Queen.validMovesFor(BoardAddress(File("A"), 0), maxFile = File("A"), maxRank = 0)
		assert(queenValidMoves.size == 0)
	}

	behavior of "King"

	it should "properly move in all directions one square" in {
		val kingValidMoves = King.validMovesFor(BoardAddress(File("C"), 2), maxFile = File("E"), maxRank = 4)
		assert(kingValidMoves.toSet == Set(
			BoardAddress(File("C"), 3),
			BoardAddress(File("C"), 1),
			BoardAddress(File("D"), 2),
			BoardAddress(File("B"), 2),
			BoardAddress(File("D"), 3),
			BoardAddress(File("D"), 1),
			BoardAddress(File("B"), 3),
			BoardAddress(File("B"), 1)
		))
	}

	it should "correctly handle being in the corner SW" in {
		val kingValidMoves = King.validMovesFor(BoardAddress(File("A"), 0), maxFile = File("C"), maxRank = 2)
		assert(kingValidMoves.toSet == Set(BoardAddress(File("A"), 1), BoardAddress(File("B"), 0), BoardAddress(File("B"), 1)))
	}

	it should "correctly handle being in the corner SE" in {
		val kingValidMoves = King.validMovesFor(BoardAddress(File("C"), 0), maxFile = File("C"), maxRank = 2)
		assert(kingValidMoves.toSet == Set(BoardAddress(File("C"), 1), BoardAddress(File("B"), 0), BoardAddress(File("B"), 1)))
	}

	it should "correctly handle 2 x 1 chess board" in {
		val kingValidMoves = King.validMovesFor(BoardAddress(File("B"), 0), maxFile = File("B"), maxRank = 0)
		assert(kingValidMoves.toSet == Set(BoardAddress(File("A"), 0)))
	}

	it should "correctly handle 1 x 1 chess board" in {
		val kingValidMoves = King.validMovesFor(BoardAddress(File("A"), 0), maxFile = File("A"), maxRank = 0)
		assert(kingValidMoves.size == 0)
	}

	behavior of "Bishop"

	it should "properly move in NE, SE, SW, NW" in {
		val bishopValidMoves = Bishop.validMovesFor(BoardAddress(File("C"), 2), maxFile = File("E"), maxRank = 4)
		assert(bishopValidMoves.toSet == Set(
			BoardAddress(File("D"), 3), BoardAddress(File("E"), 4),
			BoardAddress(File("B"), 3), BoardAddress(File("A"), 4),
			BoardAddress(File("D"), 1), BoardAddress(File("E"), 0),
			BoardAddress(File("B"), 1), BoardAddress(File("A"), 0)
		))
	}

	it should "correctly handle being in the corner" in {
		val bishopValidMoves = Bishop.validMovesFor(BoardAddress(File("A"), 0), maxFile = File("C"), maxRank = 2)
		assert(bishopValidMoves.toSet == Set(BoardAddress(File("B"), 1), BoardAddress(File("C"), 2)))
	}

	it should "correctly handle 2 x 1 chess board" in {
		val bishopValidMoves = Bishop.validMovesFor(BoardAddress(File("B"), 0), maxFile = File("B"), maxRank = 0)
		assert(bishopValidMoves.size == 0)
	}

	behavior of "Rook"

	it should "properly move in N, E, S, W" in {
		val rookValidMoves = Rook.validMovesFor(BoardAddress(File("C"), 2), maxFile = File("E"), maxRank = 4)
		assert(rookValidMoves.toSet == Set(
			BoardAddress(File("C"), 3), BoardAddress(File("C"), 4),
			BoardAddress(File("D"), 2), BoardAddress(File("E"), 2),
			BoardAddress(File("C"), 1), BoardAddress(File("C"), 0),
			BoardAddress(File("B"), 2), BoardAddress(File("A"), 2)
		))
	}

	it should "correctly handle being in the corner" in {
		val rookValidMoves = Rook.validMovesFor(BoardAddress(File("A"), 0), maxFile = File("C"), maxRank = 2)
		assert(rookValidMoves.toSet == Set(
			BoardAddress(File("A"), 1), BoardAddress(File("A"), 2),
			BoardAddress(File("B"), 0), BoardAddress(File("C"), 0))
		)
	}

	it should "correctly handle 2 x 1 chess board" in {
		val rookValidMoves = Rook.validMovesFor(BoardAddress(File("B"), 0), maxFile = File("B"), maxRank = 0)
		assert(rookValidMoves.toSet == Set(BoardAddress(File("A"), 0)))
	}

	it should "correctly handle 1 x 1 chess board" in {
		val rookValidMoves = Rook.validMovesFor(BoardAddress(File("A"), 0), maxFile = File("A"), maxRank = 0)
		assert(rookValidMoves.size == 0)
	}

	behavior of "Knight"

	it should "properly move in horsey way" in {
		val knightValidMoves = Knight.validMovesFor(BoardAddress(File("C"), 2), maxFile = File("E"), maxRank = 4)
		assert(knightValidMoves.toSet == Set(
			BoardAddress(File("D"), 4), BoardAddress(File("B"), 4),
			BoardAddress(File("E"), 3), BoardAddress(File("E"), 1),
			BoardAddress(File("D"), 0), BoardAddress(File("B"), 0),
			BoardAddress(File("A"), 3), BoardAddress(File("A"), 1)
		))
	}

	it should "correctly handle being in the corner" in {
		val knightValidMoves = Knight.validMovesFor(BoardAddress(File("A"), 0), maxFile = File("C"), maxRank = 2)
		assert(knightValidMoves.toSet == Set(BoardAddress(File("B"), 2), BoardAddress(File("C"), 1)))
	}

	it should "correctly handle 2 x 1 chess board" in {
		val knightValidMoves = Knight.validMovesFor(BoardAddress(File("B"), 0), maxFile = File("B"), maxRank = 0)
		assert(knightValidMoves.size == 0)
	}
}
