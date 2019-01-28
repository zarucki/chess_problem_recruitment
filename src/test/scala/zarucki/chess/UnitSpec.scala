package zarucki.chess

import org.scalatest._
import zarucki.chess.entities.ChessBoard

abstract class UnitSpec extends FlatSpec with Assertions with OptionValues with Inspectors {
	protected def getAll90DegreesVariants(board: ChessBoard): Set[ChessBoard] = {
		Set(
			board,
			board.rotate90degrees(),
			board.rotate180degrees(),
			board.rotate270degrees()
		)
	}
}
