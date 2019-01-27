package zarucki.chess.utils

import zarucki.chess.entities.ChessBoard

object ChessBoardExtensions {
	import scala.language.implicitConversions

	class ConsolePrintableChessBoard(chessBoard: ChessBoard) {
		def toConsoleString = ConsoleChessBoardPrinter.boardAsConsoleString(chessBoard, drawThreatenedSquares = true)
		def toConsoleStringWithoutThreats = ConsoleChessBoardPrinter.boardAsConsoleString(chessBoard, drawThreatenedSquares = false)
	}

	implicit def consolePrintableChessBoard(chessBoard: ChessBoard): ConsolePrintableChessBoard = {
		new ConsolePrintableChessBoard(chessBoard)
	}
}


