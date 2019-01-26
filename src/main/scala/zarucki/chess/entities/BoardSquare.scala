package zarucki.chess.entities

sealed trait BoardSquare

final case class Occupied(piece: Piece) extends BoardSquare
final case object FreePeaceful extends BoardSquare
final case object FreeUnderThreat extends BoardSquare

