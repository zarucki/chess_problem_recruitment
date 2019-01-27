package zarucki.chess

import zarucki.chess.entities.{ChessBoard, Piece}
import zarucki.chess.utils.ChessBoardExtensions.consolePrintableChessBoard
import zarucki.chess.utils.MeasurementUtils.measureTimeInMs

import scala.util.Try

/**
	* Example args 7 7 q q k k b b n
	* or
	* Example args 7 7 10 q q k k b b n
	* where 10 is the number of solutions you want printed
	*/
object Main extends App {
	try {
		args match {
			case Array(width: String, height: String, pieces @ _*) =>
				val (numberOfSolutions, piecesToPlace) = pieces.headOption.flatMap(h => Try(h.toInt).toOption) match {
					case Some(numberOfSolutions) => (numberOfSolutions, pieces.tail)
					case _ => (36, pieces)
				}

				val boardWidth = width.toInt
				val boardHeight = height.toInt
				val parsedPieces = piecesToPlace.toList.sorted.flatMap(pieceString => Piece.apply(pieceString.head.toUpper))
				println(s"Solving problem for: $boardWidth x $boardHeight (w x h) and " +
					s"pieces: ${parsedPieces.mkString(" ")} and printed solutions: ${numberOfSolutions}")

				val (solutions: List[ChessBoard], elapsedTimeInMs) = measureTimeInMs {
					ChessProblemSolver.solveNonThreatenProblem(
						boardWidth.toInt,
						boardHeight.toInt,
						parsedPieces
					)
				}

				solutions.take(numberOfSolutions).foreach { s => println(s.toConsoleString(drawThreats = false)) }

				println(s"Total solutions: ${solutions.size}. Found in time: ${elapsedTimeInMs} ms")
		}
	} catch {
		case exception: Exception =>
			println(exception)
			println("Example arguments: 7 7 q q k k b b n")
			println("Example arguments with 10 solutions: 7 7 10 q q k k b b n")
	}
}
