package zarucki.chess.entities

// files => columns (a-z)
// rows => ranks (0-n)
case class BoardAddress(file: File, rank: Int) {
	assert(rank >= 0, "Rank is not positive")
}

