package zarucki.chess.entities

// TODO: rotate
// TODO: board equality?
// TODO: draw thraets
// TODO: rank as only positive int?
// TODO: mutable or not?
/**
	* Board has A0 (yes 0, cause that's convenient) in lower left corner.
	*
	*     A   B   C   D   E   F   G   H
	*   ---------------------------------
	* 7 |   |   |   |   |   |   |   |   | 7
	*   ---------------------------------
	*
	* 6 |   |   |   |   |   |   |   |   | 6
	*   ---------------------------------
	*
	* 5 |   |   |   |   |   |   |   |   | 5
	*   ---------------------------------
	*
	* 4 |   |   |   |   |   |   |   |   | 4
	*   ---------------------------------
	*
	* 3 |   |   |   |   |   |   |   |   | 3
	*   ---------------------------------
	*
	* 2 |   |   |   |   |   |   |   |   | 2
	*   ---------------------------------
	*
	* 1 |   |   |   |   |   |   |   |   | 1
	*   ---------------------------------
	*
	* 0 |   |   |   |   |   |   |   |   | 0
	*   ---------------------------------
	*
	*     A   B   C   D   E   F   G   H
	*
	* @param files
	* @param ranks
	*/
class Board(files: Int, ranks: Int) {
	// TODO: some shortcut for non empty fields?
	private val board: Array[Array[Option[Piece]]] = Array.fill[Option[Piece]](ranks, files)(None)

	def placePiece(piece: Piece, address: BoardAddress): Unit = {
		//TODO: assert before that we are not exceeding
		board(address.rank)(address.file.asInt) = Some(piece)
	}

	//TODO: move it out of Board to some printer
	override def toString: String = {
		val cellContentSize = File(files - 1).asString.length
		val verticalSeparator = "|"
		val insidePad = " "

		def rankLabel(rankNumber: Int): String = s" %0${(ranks - 1).toString.length}d ".format(rankNumber)
		def fileLabel(fileNumber: Int): String = File(fileNumber).asString.padTo(cellContentSize, ' ')
		val indent = rankLabel(0).size


		def horizontalSeparator(sb: StringBuilder): StringBuilder = {
			val cellContents = (2 * insidePad.length + cellContentSize) * files
			val verticalBorders = verticalSeparator.length * (files + 1)
			sb.append(" " * indent)
				.append("-" * (cellContents + verticalBorders))
				.append("\n")
		}

		def fileLegend(sb: StringBuilder): StringBuilder = {
			sb.append(" " * indent)
				.append(
					(0 to files - 1)
						.map(fileNumber => insidePad + fileLabel(fileNumber) + insidePad)
						.mkString(" " * verticalSeparator.length, " ", " " * verticalSeparator.length)
				).append("\n")
		}

		val result = new StringBuilder()

		fileLegend(result)

		horizontalSeparator(result)

		for (r <- ranks - 1 to 0 by -1) {
			val fullRank = board(r)
				.map(pieceOpt => pieceOpt.map(piece => piece.representation.toString).getOrElse(" "))
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

// files => columns (a-z)
// rows => ranks (1-8)
case class BoardAddress(file: File, rank: Int) {
	assert(rank >= 0, "Rank is not positive")
}
