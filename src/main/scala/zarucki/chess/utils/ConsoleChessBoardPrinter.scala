package zarucki.chess.utils

import zarucki.chess.entities.{ChessBoard, File, FreeUnderThreat, Occupied}

object ConsoleChessBoardPrinter {
	def boardAsConsoleString(board: ChessBoard, drawThreatenedSquares: Boolean = true): String = {
		val cellContentSize = board.boardMaxFile.asString.length // Assuming here that all pieces are 1 char here
		val verticalSeparator = "|"
		val insidePad = " "

		def rankLabel(rankNumber: Int): String = s" %0${(board.boardMaxRank).toString.length}d ".format(rankNumber)
		def fileLabel(fileNumber: Int): String = File(fileNumber).asString.padTo(cellContentSize, ' ')
		val indent = rankLabel(0).size


		def horizontalSeparator(sb: StringBuilder): StringBuilder = {
			val fileCount = board.boardMaxFile.asInt + 1
			val cellContents = (2 * insidePad.length + cellContentSize) * fileCount
			val verticalBorders = verticalSeparator.length * (fileCount + 1)
			sb.append(" " * indent)
				.append("-" * (cellContents + verticalBorders))
				.append("\n")
		}

		def fileLegend(sb: StringBuilder): StringBuilder = {
			sb.append(" " * indent)
				.append(
					(0 to board.boardMaxFile.asInt)
						.map(fileNumber => insidePad + fileLabel(fileNumber) + insidePad)
						.mkString(" " * verticalSeparator.length, " ", " " * verticalSeparator.length)
				).append("\n")
		}

		val result = new StringBuilder()

		fileLegend(result)

		horizontalSeparator(result)

		for (r <- board.boardMaxRank to 0 by -1) {
			val fullRank = board.getWholeBoardRank(r)
				.map {
					case Occupied(piece) => piece.representation.toString
					case FreeUnderThreat if drawThreatenedSquares =>  "."
					case _ => " "
				}
				.map(cellContent => insidePad + cellContent.padTo(cellContentSize, ' ') + insidePad)
				.mkString(verticalSeparator, verticalSeparator, verticalSeparator)

			result
				.append(rankLabel(r))
				.append(fullRank)
				.append(rankLabel(r))
				.append("\n")

			horizontalSeparator(result).append("\n")
		}

		fileLegend(result)

		result.append("\n")

		result.toString()
	}
}
