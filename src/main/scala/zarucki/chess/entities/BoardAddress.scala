package zarucki.chess.entities

import File.file2Integer

// files => columns (a-z)
// rows => ranks (0-n)
case class BoardAddress(file: File, rank: Int) {
	assert(rank >= 0, "Rank is not positive")

	def getAddressDelta(toAddress: BoardAddress): AddressDelta = {
		AddressDelta(
			rankDelta = Math.abs(rank - toAddress.rank),
			fileDelta = Math.abs(file - toAddress.file)
		)
	}
}

protected case class AddressDelta(rankDelta: Int, fileDelta: Int)
