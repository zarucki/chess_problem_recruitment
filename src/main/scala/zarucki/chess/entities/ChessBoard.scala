package zarucki.chess.entities

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
	*/
trait ChessBoard {
	type Board <: ChessBoard

	def peacefulPlaces(newPiece: Piece): Seq[BoardAddress]
	def placePiece(address: BoardAddress, pieceToPlace: Piece): Board
	def tryPlacingMultipleOfSamePiece(addresses: Set[BoardAddress], pieceToPlace: Piece): Either[String, Board]

	def getWholeBoardRank(rank: Int): Seq[BoardSquare]
	def getBoardSquare(address: BoardAddress): BoardSquare
	def boardMaxRank: Int
	def boardMaxFile: File
}
