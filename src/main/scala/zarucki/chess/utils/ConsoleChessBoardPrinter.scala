package zarucki.chess.utils

import zarucki.chess.entities.{ChessBoard, File, FreeUnderThreat, Occupied}

object ConsoleChessBoardPrinter {
	private val verticalSeparator = "|"
	private val insidePad = " "
	private val newLine = "\n"
	private val threat = "."
	private val horizontalLine = "-"

	def boardAsConsoleString(board: ChessBoard, drawThreatenedSquares: Boolean = true, additionalLegend: Boolean = false): String = {
		val cellContentSize = board.boardMaxFile.asString.length // Assuming here that all pieces are 1 char here

		def rankLabel(rankNumber: Int): String = s" %0${(board.boardMaxRank).toString.length}d ".format(rankNumber)
		def fileLabel(fileNumber: Int): String = File(fileNumber).asString.padTo(cellContentSize, ' ')
		val indent = rankLabel(rankNumber = 0).size


		def horizontalSeparator(sb: StringBuilder): StringBuilder = {
			val fileCount = board.boardMaxFile.asInt + 1
			val cellContents = (2 * insidePad.length + cellContentSize) * fileCount
			val verticalBorders = verticalSeparator.length * (fileCount + 1)
			sb.append(" " * indent)
				.append(horizontalLine * (cellContents + verticalBorders))
				.append(newLine)
		}

		def fileLegend(sb: StringBuilder): StringBuilder = {
			sb.append(" " * indent)
				.append(
					(0 to board.boardMaxFile.asInt)
						.map(fileNumber => insidePad + fileLabel(fileNumber) + insidePad)
						.mkString(" " * verticalSeparator.length, " ", " " * verticalSeparator.length)
				).append(newLine)
		}

		val stringBuilder = new StringBuilder()

		if (additionalLegend) {
			fileLegend(stringBuilder)
		}

		horizontalSeparator(stringBuilder)

		for (r <- board.boardMaxRank to 0 by -1) {
			val fullRank = board.getWholeBoardRank(r)
				.map {
					case Occupied(piece) => piece.representation.toString
					case FreeUnderThreat if drawThreatenedSquares => threat
					case _ => " "
				}
				.map(cellContent => insidePad + cellContent.padTo(cellContentSize, ' ') + insidePad)
				.mkString(verticalSeparator, verticalSeparator, verticalSeparator)

			stringBuilder
				.append(rankLabel(r))
				.append(fullRank)

			if (additionalLegend) {
				stringBuilder.append(rankLabel(r))
			}

			stringBuilder.append(newLine)

			horizontalSeparator(stringBuilder).append(newLine)
		}

		fileLegend(stringBuilder)

		stringBuilder.append(newLine)

		stringBuilder.toString()
	}
}
