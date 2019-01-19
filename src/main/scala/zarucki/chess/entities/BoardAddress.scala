package zarucki.chess.entities

// TODO: rank as only positive int?
// files => columns (a-z)
// rows => ranks (1-8)
case class BoardAddress(file: File, rank: Int) {
	assert(rank >= 0, "Rank is not positive")
}

