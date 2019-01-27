package zarucki.chess.utils

import zarucki.chess.entities.ChessBoard

object ChessBoardExtensions {
	import scala.language.implicitConversions

	class ConsolePrintableChessBoard(chessBoard: ChessBoard) {
		def toConsoleString(drawThreats: Boolean = true) = {
			ConsoleChessBoardPrinter.boardAsConsoleString(chessBoard, drawThreats, additionalLegend = false)
		}
	}

	implicit def consolePrintableChessBoard(chessBoard: ChessBoard): ConsolePrintableChessBoard = {
		new ConsolePrintableChessBoard(chessBoard)
	}
}


