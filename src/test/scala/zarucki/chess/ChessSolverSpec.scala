package zarucki.chess

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import zarucki.chess.entities._

//TODO: board equality?
class ChessSolverSpec extends FlatSpec {
	it should "correctly solve 3×3 board containing 2 Kings and 1 Rook" in {
		ChessProblemSolver.solveNonThreatenProblem(3, 3, pieceConfiguration(kingCount = 2, rookCount = 1))
	}

	it should "correctly solve 4×4 board containing 2 Rooks and 4 Knights" in {
		ChessProblemSolver.solveNonThreatenProblem(4, 4, pieceConfiguration(rookCount = 2, knightCount = 4))
	}

	def pieceConfiguration(kingCount: Int = 0, queenCount: Int = 0, bishopCount: Int = 0, rookCount: Int = 0, knightCount: Int = 0): Seq[Piece] = {
		(0 until kingCount).map(_ => King()) ++
			(0 until queenCount).map(_ => Queen()) ++
			(0 until bishopCount).map(_ => Bishop()) ++
			(0 until rookCount).map(_ => Rook()) ++
			(0 until knightCount).map(_ => Knight())
	}
}
