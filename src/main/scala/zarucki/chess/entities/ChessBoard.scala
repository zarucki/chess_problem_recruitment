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

	def peacefulPlaces: Seq[BoardAddress]
	def peacefulPlacesForPiece(newPiece: Piece): Seq[BoardAddress]
	def placePiece(pieceToPlace: Piece, newPieceAddress: BoardAddress): Board
	def tryPlacingMultipleOfSamePiece(pieceToPlace: Piece, newPieceAddresses: BoardAddress*): Either[String, Board]

	def getWholeBoardRank(rank: Int): Seq[BoardSquare]
	def getBoardSquare(address: BoardAddress): BoardSquare
	def boardMaxRank: Int
	def boardMaxFile: File

	def rotate90degrees(): VectorChessBoard
	def rotate180degrees(): VectorChessBoard
	def rotate270degrees(): VectorChessBoard
}
