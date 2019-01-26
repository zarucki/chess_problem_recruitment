package zarucki.chess.utils

import zarucki.chess.entities.ChessBoard

object ChessBoardExtensions {
	class ConsolePrintableChessBoard(chessBoard: ChessBoard) {
		def toConsoleString = ConsoleChessBoardPrinter.boardAsConsoleString(chessBoard, drawThreatenedSquares = true)
		def toConsoleStringWithoutThreats = ConsoleChessBoardPrinter.boardAsConsoleString(chessBoard, drawThreatenedSquares = false)
	}

	implicit def consolePrintableChessBoard(chessBoard: ChessBoard) = new ConsolePrintableChessBoard(chessBoard)
}


